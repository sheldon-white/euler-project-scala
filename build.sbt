name := "euler-project-scala"
 
version := "1.0"
 
scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
)

unmanagedResourceDirectories in Compile <++= baseDirectory { base =>
    Seq( base / "src/main/resources" )
}
