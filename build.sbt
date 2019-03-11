name := "CatsPlayground"

version := "0.1"

scalaVersion := "2.12.6"

// https://mvnrepository.com/artifact/org.typelevel/cats-core
libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"

scalacOptions := Seq(
  "-Ypartial-unification"
)