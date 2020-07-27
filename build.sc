import mill._
import mill.modules.Util
import $ivy.`com.lihaoyi::mill-contrib-bsp:$MILL_VERSION`
import scalalib._

object gitmirror extends ScalaModule {
  def scalaVersion = "2.13.2"
  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections:latest.integration",
    ivy"com.lihaoyi::requests:latest.integration",
    ivy"com.lihaoyi::upickle:latest.integration",
    ivy"com.lihaoyi::os-lib:latest.integration"
  )
}
