name         := "SparkBenchmarks"
version      := "0.1"
scalaVersion := "2.13.16"
// libraryDependencies += "org.apache.spark" %% "spark-sql" % "4.0.0"
libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-sql" % "4.0.0",
  "joda-time" % "joda-time"    % "2.12.5",
  "org.joda"  % "joda-convert" % "2.2.3"
)
