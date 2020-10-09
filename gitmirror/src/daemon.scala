package gitmirror

object daemon extends App {
  while (true) {
    /* reload config when running. */
    val config = new Config(os.Path(System.getenv("GIT_MIRROR_CONFIG_FILE")))
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
