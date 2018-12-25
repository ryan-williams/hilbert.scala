default(
  group("com.runsascoded"),
  `2.12`.version := "2.12.8",
  `2.12`.only
)

lazy val core =
  cross
    .settings(
      dep(
        iterators % "2.2.0",
        shapeless_utils % "1.5.1"
      )
    )
lazy val `core-x` = core.x

import scalajs.{ css, dom, react }

lazy val web =
  project
    .settings(
      versions(
        dom → "0.9.6"
      ),
      react.  version := "1.3.1",
      react.jsVersion := "16.5.1",
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

lazy val hilbert = root(`core-x`, web)
