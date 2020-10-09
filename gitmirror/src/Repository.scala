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
  protected lazy val logger: Logger = Logger(LoggerFactory.getLogger(s"$user/$repo"))

  def processOutputTostringBuilder(stringBuilder: StringBuilder): ProcessOutput.ReadBytes = os.ProcessOutput.ReadBytes(
    (buf, _) => stringBuilder.append(new String(buf, StandardCharsets.UTF_8))
  )

  private def now = (System.currentTimeMillis() / 1000).toString

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

  def gitlabGroups: Map[String, Int] = ujson.read(
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

  def getHash: String = os.list(wd / "objects" / "pack").filter(_.ext == "idx").map(_.last.slice(5, 45)).mkString

  def githubSSHUrl = s"git@$githubUrl:$user/$repo.git"

  def gitlabSSHUrl = s"git@$gitlabUrl:$user/$repo.git"

  def mirror: Boolean = {
    if (os.isFile(wd / "config")) {
      githubFetch
    } else {
      githubClone
      gitlabCreateProject(user, repo)
    }
    gitlabPush
  }


  def githubClone: Boolean = {
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

  def githubFetch: Boolean = {
    val oldHash = getHash
    val log = new StringBuilder
    if (os.isDir(wd)) {
      if (os.proc("git", "remote", "update")
        .call(wd, env = githubSSHEnv, check = false, stdout = processOutputTostringBuilder(log), mergeErrIntoOut = true)
        .exitCode != 0) {
        logger.error(s"failed with ${log.toString}!")
        false
      } else {
        os.write.over(wd / ".fetchTimestamp", now)
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

  def gitlabPush: Boolean = {
    val log = new StringBuilder
    if (os.proc("git", "push", "--mirror", "--force", gitlabSSHUrl)
      .call(wd, env = gitlabSSHEnv, check = false, stdout = processOutputTostringBuilder(log), mergeErrIntoOut = true)
      .exitCode == 0) {
      logger.info(log.toString)
      os.write.over(wd / ".pushTimestamp", now)
      true
    } else {
      logger.error(s"failed with ${log.toString}.")
      false
    }
  }

  def sync: Boolean = if (githubFetch) gitlabPush else false
}
