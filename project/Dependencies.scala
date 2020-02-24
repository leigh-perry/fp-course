import sbt._

object Dependencies {
  object Version {
    val kindProjectorVersion = "0.10.3"

    val zio = "1.0.0-RC17"
    val ziocats = "2.0.0.0-RC10"
  }

  val zio = "dev.zio" %% "zio" % Version.zio
  val ziocats = "dev.zio" %% "zio-interop-cats" % Version.ziocats

  val zioTest = "dev.zio" %% "zio-test" % Version.zio
  val zioTestSbt = "dev.zio" %% "zio-test-sbt" % Version.zio
}
