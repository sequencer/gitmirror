package gitmirror

import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging._
import org.slf4j.LoggerFactory
import os.{Path, ProcessOutput}
import requests.Response

case class Repository(githubUrl: String,
                      gitlabUrl: String,
                      githubToken: String,
                      gitlabToken: String,
                      githubSSHKey: String,
                      gitlabSSHKey: String,
                      mirrorDirectory: os.Path,
                      user: String,
                      repo: String) {
  def name = s"$user/$repo"

  val githubAPI = s"https://api.$githubUrl"
  val gitlabAPI = s"http://$gitlabUrl/api/v4"
  val gitlabAPIHeaders = Map("Private-Token" -> s"$gitlabToken", "Content-Type" -> "application/json")
  val githubAPIHeaders = Map("Authorization" -> s"token $githubToken")

  protected lazy val logger: Logger = Logger(LoggerFactory.getLogger(s"$user/$repo"))

  def processOutputToStringBuilder(stringBuilder: StringBuilder): ProcessOutput.ReadBytes = os.ProcessOutput.ReadBytes(
    (buf, _) => stringBuilder.append(new String(buf, StandardCharsets.UTF_8))
  )

  private def now = (System.currentTimeMillis() / 1000).toString

  lazy val githubRemotePushedAt: Long = ujson.read(requests.get(
    githubAPI + s"/repos/$user/$repo",
    headers = githubAPIHeaders
  ).bytes)("pushed_at").strOpt.map(
    java.time.Instant.parse(_).getEpochSecond
  ).getOrElse(0)

  lazy val githubLocalFetchedAt: Long = os.read(wd / ".fetchTimestamp").toLong

  lazy val gitlabLocalPushedAt: Long = os.read(wd / ".pushTimestamp").toLong

  def gitlabCreateGroup(name: String): Response = requests.post(gitlabAPI + "/groups", data = ujson.write(Map(
    "name" -> name,
    "path" -> name,
    "visibility" -> "public",
    "description" -> s"Mirror of $name"
  )),
    headers = gitlabAPIHeaders,
    check = false
  )

  def gitlabCreateProject(group: String, name: String): Response = {
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

  lazy val gitlabGroups: Map[String, Int] = ujson.read(
    requests.get(
      gitlabAPI + "/groups",
      headers = gitlabAPIHeaders
    ).bytes
  )
    .arr
    .map {
      u => u("name").str -> u("id").num.toInt
    }.toMap

  val wd: Path = mirrorDirectory / user / repo

  val githubSSHEnv = Map(
    "GIT_SSH_COMMAND" -> s"ssh -i $githubSSHKey"
  )

  val gitlabSSHEnv = Map(
    "GIT_SSH_COMMAND" -> s"ssh -i $gitlabSSHKey"
  )

  lazy val githubSSHUrl = s"git@$githubUrl:$user/$repo.git"

  lazy val gitlabSSHUrl = s"git@$gitlabUrl:$user/$repo.git"

  lazy val mirror: Boolean = {
    if (os.isFile(wd / "config")) {
      githubFetch
    } else {
      githubClone
      gitlabCreateProject(user, repo)
    }
    gitlabPush
  }


  lazy val githubClone: Boolean = {
    os.remove.all(wd)
    os.makeDir.all(wd)
    val log = new StringBuilder
    if (os.proc("git", "clone", "--bare", githubSSHUrl, wd.toString)
      .call(wd, env = githubSSHEnv, check = false, stdout = processOutputToStringBuilder(log), mergeErrIntoOut = true)
      .exitCode == 0) {
      os.write.over(wd / ".pushTimestamp", "0")
      os.write.over(wd / ".fetchTimestamp", now)
      logger.info(log.toString)
      true
    } else {
      logger.error(s"failed with ${log.toString}.")
      false
    }
  }

  lazy val githubFetch: Boolean = {
    val log = new StringBuilder
    if (os.isDir(wd)) {
      if (githubRemotePushedAt > githubLocalFetchedAt) {
        if (os.proc("git", "remote", "update")
          .call(wd, env = githubSSHEnv, check = false, stdout = processOutputToStringBuilder(log), mergeErrIntoOut = true)
          .exitCode != 0) {
          logger.error(s"failed with ${log.toString}!")
          false
        } else {
          os.write.over(wd / ".fetchTimestamp", now)
          true
        }
      } else false
    } else {
      logger.error(s"target dir ${wd.toString} not exist fall back to clone.")
      githubClone
    }
  }

  lazy val gitlabPush: Boolean = {
    val log = new StringBuilder
    if (os.proc("git", "push", "--mirror", "--force", gitlabSSHUrl)
      .call(wd, env = gitlabSSHEnv, check = false, stdout = processOutputToStringBuilder(log), mergeErrIntoOut = true)
      .exitCode == 0) {
      logger.info(log.toString)
      os.write.over(wd / ".pushTimestamp", now)
      true
    } else {
      logger.error(s"failed with ${log.toString}.")
      false
    }
  }

  lazy val sync: Boolean = if (githubFetch) gitlabPush else false
}
