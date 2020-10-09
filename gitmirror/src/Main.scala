package gitmirror

import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging._
import org.slf4j.LoggerFactory
import ujson._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport

case class Repository(githubUrl: String,
                      gitlabUrl: String,
                      githubToken: String,
                      gitlabToken: String,
                      githubSSHKey: String,
                      gitlabSSHKey: String,
                      mirrorDirectory: os.Path,
                      user: String,
                      repo: String) {
  val githubAPI = s"https://api.$githubUrl"
  val gitlabAPI = s"http://$gitlabUrl/api/v4"
  val gitlabAPIHeaders = Map("Private-Token" -> s"$gitlabToken", "Content-Type" -> "application/json")
  protected lazy val logger: Logger = Logger(LoggerFactory.getLogger(s"$user/$repo"))

  def processOutputTostringBuilder(stringBuilder: StringBuilder) = os.ProcessOutput.ReadBytes(
    (buf, _) => stringBuilder.append(new String(buf, StandardCharsets.UTF_8))
  )

  def gitlabCreateGroup(name: String) = requests.post(gitlabAPI + "/groups", data = ujson.write(Map(
    "name" -> name,
    "path" -> name,
    "visibility" -> "public",
    "description" -> s"Mirror of $name"
  )),
    headers = gitlabAPIHeaders,
    check = false
  )

  def gitlabCreateProject(group: String, name: String) = {
    gitlabCreateGroup(group)
    requests.post(
      gitlabAPI + s"/projects",
      data = ujson.write(Map(
        "name" -> name,
        "namespace_id" -> gitlabGroups(group).toString,
        "visibility" -> "public",
        "description" -> s"Mirror of $name"
      )),
      headers = gitlabAPIHeaders,
      check = false
    )
  }

  def gitlabGroups = ujson.read(
    requests.get(
      gitlabAPI + "/groups",
      headers = gitlabAPIHeaders
    ).bytes
  )
    .arr
    .map {
      u => u("name").str -> u("id").num.toInt
    }.toMap

  val wd = mirrorDirectory / user / repo

  val githubSSHEnv = Map(
    "GIT_SSH_COMMAND" -> s"ssh -i $githubSSHKey"
  )

  val gitlabSSHEnv = Map(
    "GIT_SSH_COMMAND" -> s"ssh -i $gitlabSSHKey"
  )

  def getHash = os.list(wd / "objects" / "pack").filter(_.ext == "idx").map(_.last.slice(5, 45)).mkString

  def githubSSHUrl = s"git@$githubUrl:$user/$repo.git"

  def gitlabSSHUrl = s"git@$gitlabUrl:$user/$repo.git"

  def mirror =
    if (if (os.isFile(wd / "config")) {
      githubFetch
    } else {
      githubClone
      gitlabCreateProject(user, repo)
      true
    }) gitlabPush

  def githubClone = {
    os.remove.all(wd)
    os.makeDir.all(wd)
    val log = new StringBuilder
    os.write.over(wd / ".fetchTimestamp", "0")
    os.write.over(wd / ".pushTimestamp", "0")
    if (os.proc("git", "clone", "--bare", githubSSHUrl, wd.toString)
      .call(wd, env = githubSSHEnv, check = false, stdout = processOutputTostringBuilder(log), mergeErrIntoOut = true)
      .exitCode == 0) {
      logger.info(log.toString)
      true
    } else {
      logger.error(s"failed with ${log.toString}.")
      false
    }
  }

  def githubFetch = {
    val oldHash = getHash
    val log = new StringBuilder
    if (os.isDir(wd)) {
      if (os.proc("git", "remote", "update")
        .call(wd, env = githubSSHEnv, check = false, stdout = processOutputTostringBuilder(log), mergeErrIntoOut = true)
        .exitCode != 0) {
        logger.error(s"failed with ${log.toString}!")
        false
      } else {
        os.write.over(wd / ".fetchTimestamp", System.currentTimeMillis().toString)
        if (getHash != oldHash) {
          logger.info("need update.")
          logger.info(log.toString)
          true
        }
        else
          false
      }
    } else {
      logger.error(s"target dir ${wd.toString} not exist fall back to clone.")
      githubClone
    }
  }

  def gitlabPush = {
    val log = new StringBuilder
    if (os.proc("git", "push", "--mirror", "--force", gitlabSSHUrl)
      .call(wd, env = gitlabSSHEnv, check = false, stdout = processOutputTostringBuilder(log), mergeErrIntoOut = true)
      .exitCode == 0) {
      logger.info(log.toString)
      os.write.over(wd / ".pushTimestamp", System.currentTimeMillis().toString)
      true
    } else {
      logger.error(s"failed with ${log.toString}.")
      false
    }
  }

  def sync = if (githubFetch) gitlabPush else false
}

class Config(localConfigPath: os.Path) {
  def pages(url: String) = {
    val pattern = """.*page=(\d+)>.*""".r
    val pattern(a) = requests.get(url + "?q=addClass+user:mozilla", headers = githubAPIHeaders).headers.get("link") match {
      case Some(link) => link.head.split(",").filter(_.contains("last")).head
      case None => "page=0>"
    }
    a.toInt
  }

  def repos(origination: String) = {
    val url = githubAPI + s"/orgs/$origination/repos"
    (0 to pages(url)).flatMap { page =>
      ujson.read(requests.get(
        url + s"?page=$page",
        headers = githubAPIHeaders
      ).bytes).arr.map(r =>
        Repository(
          githubUrl = githubUrl,
          gitlabUrl = gitlabUrl,
          githubToken = githubToken,
          gitlabToken = gitlabToken,
          githubSSHKey = githubSSHKey,
          gitlabSSHKey = gitlabSSHKey,
          mirrorDirectory = mirrorDirectory,
          user = origination,
          repo = r("name").str
        )
      )
    }
  }.toVector

  def repos(userrepo: Array[String]) = {
    require(userrepo.length == 2)
    val user = userrepo.head
    val repo = userrepo.last
    Repository(
      githubUrl = githubUrl,
      gitlabUrl = gitlabUrl,
      githubToken = githubToken,
      gitlabToken = gitlabToken,
      githubSSHKey = githubSSHKey,
      gitlabSSHKey = gitlabSSHKey,
      mirrorDirectory = mirrorDirectory,
      user = user,
      repo = repo
    )
  }

  def sshString(str: String) = s"-----BEGIN OPENSSH PRIVATE KEY-----\n${str}\n-----END OPENSSH PRIVATE KEY-----\n"

  val localConfig = ujson.read(os.read(localConfigPath)).obj
  val remoteConfig = localConfig.get("remoteConfig").map(u => ujson.read(requests.get(u.str).text)).obj

  /* local config only */
  lazy val githubUrl = localConfig.getOrElse("githubUrl", Str("github.com")).str
  lazy val gitlabUrl = localConfig.getOrElse("gitlabUrl", Str("localhost")).str
  lazy val githubToken = localConfig.getOrElse("githubToken", Str("")).str
  lazy val gitlabToken = localConfig.getOrElse("gitlabToken", Str("")).str
  lazy val action = localConfig.getOrElse("action", Str("clone")).str
  lazy val threads = localConfig.getOrElse("threads", Num(64.0)).num.toInt
  lazy val githubSSHKey = os.temp(sshString(localConfig.getOrElse("githubSSHKey", Str("")).str)).toString
  lazy val gitlabSSHKey = os.temp(sshString(localConfig.getOrElse("gitlabSSHKey", Str("")).str)).toString
  lazy val mirrorDirectory = os.Path(localConfig.getOrElse("mirrorDirectory", Str("/tmp/gitmirror")).str)

  /* remote will override local. */
  lazy val originationRepos = (localConfig ++ remoteConfig).getOrElse("origination", Arr()).arr.map(_.str).flatMap(repos)
  lazy val standaloneRepos = (localConfig ++ remoteConfig).getOrElse("repository", Arr()).arr.map(r => repos(r.str.split('/')))

  lazy val githubAPIHeaders = Map("Authorization" -> s"token $githubToken")
  lazy val githubAPI = s"https://api.$githubUrl"
  lazy val repos = (originationRepos ++ standaloneRepos).par
}

object Main extends App {
  val config = new Config(os.Path(args(0), os.pwd))
  val forkJoinPool = new java.util.concurrent.ForkJoinPool(config.threads)
  config.repos.tasksupport = new ForkJoinTaskSupport(forkJoinPool)
  config.repos.map(r => config.action match {
    case "clone" => r.githubClone
    case "fetch" => r.githubFetch
    case "push" => r.gitlabPush
    case "mirror" => r.mirror
    case "sync" => r.sync
  })
}
