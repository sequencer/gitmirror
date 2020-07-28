package gitmirror

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

  def githubSSHUrl = s"git@$githubUrl:$user/$repo.git"

  def gitlabSSHUrl = s"git@$gitlabUrl:$user/$repo.git"

  def gitClone = {
    os.makeDir.all(wd)
    os.proc("git", "clone", "--bare", githubSSHUrl, wd.toString).call(wd, env = Map(
      "GIT_SSH_COMMAND" -> s"ssh -i $githubSSHKey -o IdentitiesOnly=yes"
    ))
  }

  def mirror = {
    if (!os.isDir(wd)) {
      gitClone
      gitlabCreateProject(user, repo)
    }
    sync
  }

  def sync = {
    os.proc("git", "fetch", "--all").call(wd, env = Map(
      "GIT_SSH_COMMAND" -> s"ssh -i $githubSSHKey -o IdentitiesOnly=yes"
    ))
    os.proc("git", "push", "--mirror", "--force", gitlabSSHUrl).call(wd, env = Map(
      "GIT_SSH_COMMAND" -> s"ssh -i $gitlabSSHKey -o IdentitiesOnly=yes"
    ))
  }
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
          githubUrl = config("githubUrl").str,
          gitlabUrl = config("gitlabUrl").str,
          githubToken = config("githubToken").str,
          gitlabToken = config("gitlabToken").str,
          githubSSHKey = os.temp(config("githubSSHKey").str).toString,
          gitlabSSHKey = os.temp(config("gitlabSSHKey").str).toString,
          mirrorDirectory = os.Path(config("mirrorDirectory").str),
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
      githubUrl = config("githubUrl").str,
      gitlabUrl = config("gitlabUrl").str,
      githubToken = config("githubToken").str,
      gitlabToken = config("gitlabToken").str,
      githubSSHKey = os.temp(config("githubSSHKey").str).toString,
      gitlabSSHKey = os.temp(config("gitlabSSHKey").str).toString,
      mirrorDirectory = os.Path(config("mirrorDirectory").str),
      user = user,
      repo = repo
    )
  }

  val config = ujson.read(os.read(os.Path(args(0), os.pwd)))
  val forkJoinPool = new java.util.concurrent.ForkJoinPool(config("threads").numOpt.getOrElse(32.0).toInt)
  val githubUrl = config("githubUrl").str
  val githubAPI = s"https://api.$githubUrl"
  val githubToken = config("githubToken").str
  val githubAPIHeaders = Map("Authorization" -> s"token $githubToken")
  val tasks = (config("origination").arr.map(_.str).flatMap(repos) ++ config("repository").arr.map(_.str).map(r => repos(r.split('/')))).par
  tasks.tasksupport = new ForkJoinTaskSupport(forkJoinPool)
  tasks.map(r => config("action").strOpt.getOrElse("clone") match {
    case "clone" => r.gitClone
    case "mirror" => r.mirror
    case "sync" => r.sync
  })
}
