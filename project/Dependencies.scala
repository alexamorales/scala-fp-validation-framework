import sbt._

object Dependencies {

  val zioVersion                     = "1.0.10"
  val zioCatsVersion                 = "2.5.1.0"
  val zioConfigVersion               = "1.0.6"
  val catsVersion                    = "2.6.1"
  val catsEffectVersion              = "2.5.1"
  val circeVersion                   = "0.14.1"

  lazy val validation = Seq(
    "dev.zio"                       %% "zio"                               % zioVersion,
    "dev.zio"                       %% "zio-interop-cats"                  % zioCatsVersion,
    "dev.zio"                       %% "zio-config-typesafe"               % zioConfigVersion,
    "org.typelevel"                 %% "cats-core"                         % catsVersion,
    "org.typelevel"                 %% "cats-effect"                       % catsEffectVersion
  )
}
