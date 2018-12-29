default(
  group("com.runsascoded"),
  `2.12`.version := "2.12.8",
  `2.12`.only
)

lazy val cli =
  project
    .settings(

    )
    .dependsOn(core.jvm)

lazy val core =
  cross
    .settings(
      dep(
        iterators % "2.2.0",
        shapeless_utils % "1.5.1"
      )
    )
lazy val `core-x` = core.x

import scalajs.{ css, react }

lazy val web =
  project
    .settings(
      scalaJSUseMainModuleInitializer := true,
      react,
      dep(
        css.react
      ),
    )
    .enablePlugins(JS, ScalaJSBundlerPlugin)
    .dependsOn(
      core.js
    )

lazy val hilbert = root(cli, `core-x`, web)
