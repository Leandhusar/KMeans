// build.sc
import mill._, scalalib._

object foo extends ScalaModule{
  def scalaVersion = "2.13.2"
  object test extends Tests{
    def testFrameworks = Seq("utest.runner.Framework")
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.7.4"
    )
  }
}
