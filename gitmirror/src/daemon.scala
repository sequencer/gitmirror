package gitmirror

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object daemon extends App {
  val config = new Config(os.Path(System.getenv("GIT_MIRROR_CONFIG_FILE")))
  Future {
    while (true) {
      config.repos.map(r => config.action match {
        case "clone" => r.githubClone
        case "fetch" => r.githubFetch
        case "push" => r.gitlabPush
        case "mirror" => r.mirror
        case "sync" => r.sync
      })
      Thread.sleep(config.timer)
    }
  }
}
