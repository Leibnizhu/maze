
scalaVersion := "3.3.1"
name := "Maze"
version := "0.1-SNAPSHOT"
Test / parallelExecution := false

val log4j2Version = "2.24.1"

unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "resources"

libraryDependencies ++= Seq(
  // 日志相关
  //  "org.jboss.slf4j" % "slf4j-jboss-logging" % "1.2.1.Final",
  "org.apache.logging.log4j" % "log4j-api" % log4j2Version,
  "org.apache.logging.log4j" % "log4j-core" % log4j2Version,
  "org.apache.logging.log4j" % "log4j-slf4j2-impl" % log4j2Version,
  "org.slf4j" % "slf4j-api" % "2.0.13",
  // cli解析命令
  "org.jline" % "jline" % "3.26.2",
  // jackson
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.18.0",
  // 图形界面
  "org.scalafx" %% "scalafx" % "21.0.0-R32",
  "org.openjfx" % "javafx-base" % "21" % "compile",
  "org.openjfx" % "javafx-graphics" % "21" % "compile",
  "org.openjfx" % "javafx-controls" % "21" % "compile",
  "org.openjfx" % "javafx-fxml" % "21" % "compile",

  // 其他测试相关
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.mockito" % "mockito-core" % "5.11.0" % Test
)

// 如果你使用的是 JavaFX 17，确保添加 VM 选项
scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature")
fork := true

lazy val app = (project in file("."))
  .settings(
    assembly / mainClass := Some("io.github.leibnizhu.maze.App"),
    assembly / assemblyJarName := "Maze.jar",
  )

assembly / assemblyMergeStrategy := {
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
