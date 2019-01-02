default(
  group("com.runsascoded"),
  `2.12`.version := "2.12.8"
)

lazy val cli = project.dependsOn(core.jvm)

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
      react,
      webpackBundlingMode := BundlingMode.LibraryAndApplication(),
      dep(
        css.react,
        hammerlab.io % "5.2.1"
      ),
    )
    .enablePlugins(JS, ScalaJSBundlerPlugin)
    .dependsOn(
      core.js,
      worker
    )

lazy val `web-worker` =
  project
    .in(new File("web/worker"))
    .settings(
      scalaJSUseMainModuleInitializer := true
    )
    .enablePlugins(ScalaJSPlugin)
    .dependsOn(web, worker)

lazy val worker =
  project
    .settings(
      dep(
        boopickle,
        magnolia,
        scalajs.dom
      )
    )
    .enablePlugins(JS)

lazy val hilbert = root(cli, `core-x`, web, `web-worker`, worker)
