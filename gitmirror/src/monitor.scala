package gitmirror

import cask._

import scala.collection.mutable
import scala.concurrent.Future

object monitor extends MainRoutes {
  /* Everytime calling config, force reconstruct everything. */
  def config = new Config(os.Path(System.getenv("GIT_MIRROR_CONFIG_FILE")))

  val lastFetches: mutable.Map[String, Long] = mutable.Map[String, Long]()
  val lastPushes: mutable.Map[String, Long] = mutable.Map[String, Long]()
  val lastUpdates: mutable.Map[String, Long] = mutable.Map[String, Long]()

  /* Init caches with 0. */
  config.repos.foreach { r =>
    lastFetches(r.name) = 0
    lastPushes(r.name) = 0
    lastUpdates(r.name) = 0
  }

  /* update automatically. */
  Future {
    while (true) {
      updateRemoteTimestamp
      updateLocalTimestamp
      Thread.sleep(config.timer)
    }
  }

  @cask.get("/timeout")
  def timeout = {
    lastUpdates.keys.mkString("\n")
  }

  @cask.get("/update/remote_timestamp")
  def updateRemoteTimestamp = config.repos.foreach { r =>
    lastUpdates(r.name) = r.githubRemotePushedAt
  }

  @cask.get("/update/local_timestamp")
  def updateLocalTimestamp = os.walk(config.mirrorDirectory, maxDepth = 2).foreach { p =>
    val segments = p.segments.toVector
    val name = s"${segments(p.segmentCount - 3)}/${segments(p.segmentCount - 2)}}"
    p.last match {
      case ".fetchTimestamp" => lastFetches(name) = os.read(p).toLong
      case ".pushTimestamp" => lastPushes(name) = os.read(p).toLong
    }
  }
}