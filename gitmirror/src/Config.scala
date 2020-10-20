package gitmirror

import ujson._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray

class Config(localConfigPath: os.Path) {
  def pages(url: String) = {
    val pattern = """.*page=(\d+)>.*""".r
    val pattern(a) = requests.get(url + "?q=addClass+user:mozilla", headers = githubAPIHeaders).headers.get("link") match {
      case Some(link) => link.head.split(",").filter(_.contains("last")).head
      case None => "page=0>"
    }
    a.toInt
  }

  def repo(origination: String) = {
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

  def repo(userrepo: Array[String]) = {
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
  val remoteConfig = localConfig.get("remoteConfig").map(u => ujson.read(requests.get(u.str).bytes)).get.obj
  /* local config only */
  lazy val githubUrl = localConfig.getOrElse("githubUrl", Str("github.com")).str
  lazy val gitlabUrl = localConfig.getOrElse("gitlabUrl", Str("localhost")).str
  lazy val githubToken = localConfig.getOrElse("githubToken", Str("")).str
  lazy val gitlabToken = localConfig.getOrElse("gitlabToken", Str("")).str
  lazy val action = localConfig.getOrElse("action", Str("clone")).str
  lazy val threads = localConfig.getOrElse("threads", Num(64.0)).num.toInt
  lazy val timer = localConfig.getOrElse("timer", Num(600)).num.toLong
  lazy val githubSSHKey = os.temp(sshString(localConfig.getOrElse("githubSSHKey", Str("")).str)).toString
  lazy val gitlabSSHKey = os.temp(sshString(localConfig.getOrElse("gitlabSSHKey", Str("")).str)).toString
  lazy val mirrorDirectory = os.Path(localConfig.getOrElse("mirrorDirectory", Str("/tmp/gitmirror")).str)
  lazy val warningTimeout = localConfig.getOrElse("warningTimeout", Num(3600)).num.toInt

  /* remote will override local. */
  lazy val originationRepos = (localConfig ++ remoteConfig).getOrElse("origination", Arr()).arr.map(_.str).flatMap(repo)
  lazy val standaloneRepos = (localConfig ++ remoteConfig).getOrElse("repository", Arr()).arr.map(r => repo(r.str.split('/')))

  lazy val githubAPIHeaders = Map("Authorization" -> s"token $githubToken")
  lazy val githubAPI = s"https://api.$githubUrl"

  lazy val repos: ParArray[Repository] = {
    val r: ParArray[Repository] = (originationRepos ++ standaloneRepos).par
    r.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(threads))
    r
  }

  def run: ParArray[Boolean] = {
    repos.map(r => action match {
      case "clone" => r.githubClone
      case "fetch" => r.githubFetch
      case "push" => r.gitlabPush
      case "mirror" => r.mirror
      case "sync" => r.sync
    })
  }
}
