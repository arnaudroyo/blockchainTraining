ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Scala Seed Project",
    
    // Ajouter les dépendances web3j ici
    libraryDependencies ++= Seq(
      "org.web3j" % "core" % "4.9.5",
      "org.web3j" % "crypto" % "4.9.5",
      "org.web3j" % "utils" % "4.9.5",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test
    )
  )
