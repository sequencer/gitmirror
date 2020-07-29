package gitmirror

import java.util.Calendar

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

  def gitlabCreateGroup(name: String) =
    requests.post(gitlabAPI + "/groups", data = ujson.write(Map(
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
    val log = os.temp(Calendar.getInstance().getTime.toString)
    os.remove.all(wd)
    os.makeDir.all(wd)
    if (os.proc("git", "clone", "--bare", githubSSHUrl, wd.toString).call(wd, env = githubSSHEnv, check = false, stdout = log).exitCode != 0) println(s"$user/$repo clone failed.")
    os.move(log, wd / "cloneLog.txt")
  }

  def githubFetch = {
    val oldHash = getHash
    val log = wd / "fetchLog.txt"
    os.write.append(log, Calendar.getInstance().getTime.toString)
    if (os.proc("git", "remote", "update").call(wd, env = githubSSHEnv, check = false, stdout = log).exitCode != 0) println(s"$user/$repo fetch failed.")
    os.write.append(log, "-" * 64 + "\n\n\n")

    getHash != oldHash
  }

  def gitlabPush = {
    val log = wd / "pushLog.txt"
    os.write.append(log, Calendar.getInstance().getTime.toString)
    if (os.proc("git", "push", "--mirror", "--force", gitlabSSHUrl).call(wd, env = gitlabSSHEnv, check = false, stdout = log).exitCode != 0) println(s"$user/$repo push failed.")
    os.write.append(log, "-" * 64 + "\n\n\n")
  }

  def sync = if (githubFetch) {
    println(s"$user/$repo need update")
    gitlabPush
  } else
    println(s"$user/$repo no need update")
}


object Main extends App {
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
    require(userrepo.size == 2)
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

  val config = ujson.read(os.read(os.Path(args(0), os.pwd))).obj

  lazy val githubUrl = config.getOrElse("githubUrl", Str("github.com")).str
  lazy val gitlabUrl = config.getOrElse("gitlabUrl", Str("localhost")).str
  lazy val githubToken = config.getOrElse("githubToken", Str("")).str
  lazy val gitlabToken = config.getOrElse("gitlabToken", Str("")).str
  lazy val githubSSHKey = os.temp(sshString(config.getOrElse("githubSSHKey", Str("")).str)).toString
  lazy val gitlabSSHKey = os.temp(sshString(config.getOrElse("gitlabSSHKey", Str("")).str)).toString
  lazy val mirrorDirectory = os.Path(config.getOrElse("mirrorDirectory", Str("/tmp/gitmirror")).str)
  lazy val originationRepos = config.getOrElse("origination", Arr()).arr.map(_.str).flatMap(repos)
  lazy val standaloneRepos = config.getOrElse("repository", Arr()).arr.map(r => repos(r.str.split('/')))
  lazy val tasks = (originationRepos ++ standaloneRepos).par
  lazy val action = config.getOrElse("action", Str("clone")).str
  lazy val threads = config.getOrElse("threads", Num(64.0)).num.toInt
  lazy val githubAPIHeaders = Map("Authorization" -> s"token $githubToken")
  lazy val githubAPI = s"https://api.$githubUrl"

  val forkJoinPool = new java.util.concurrent.ForkJoinPool(threads)
  tasks.tasksupport = new ForkJoinTaskSupport(forkJoinPool)
  tasks.map(r => action match {
    case "clone" => r.githubClone
    case "fetch" => r.githubFetch
    case "push" => r.gitlabPush
    case "mirror" => r.mirror
    case "sync" => r.sync
  })
}
