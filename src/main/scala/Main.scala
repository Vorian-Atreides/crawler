import crawler.robot.Robot

object Main extends App {
  val tokens = Array(
    "Allow: /",
    "User-Agent: toto",
    "Allow: /",
    "User-Agent: *",
    "User-Agent: google-bot",
    "",
    "# justa  test",
    "Allow: /",
    "Sitemap: hello",
    "Disallow: /search #with a comment",
    "invalid-line",
    "Allow: /something:else",
    "Hello: World",
    "Allow: /search/about",
    "User-Agent: sombebody",
  )
  val fullcontent = tokens.fold("")((a, b) => s"${a}${b}\n")
//
//  val results = LexicalParser.parse(fullcontent)
//  results match {
//    case Left(data) => data.foreach(println)
//    case Right(data) => data.foreach(println)
//  }
//
//  val tree = SyntacticParser.buildRobot(results.right.get)
  val robot = Robot.fromString(fullcontent)
  println(robot)

  val googleDirectives = robot.isAllowed("google-bot")
  println(googleDirectives("/search"))
  println(googleDirectives("/search/about/somehting"))
  println(googleDirectives("/toto"))

  val totoDirectives = robot.isAllowed("toto")
  println(totoDirectives("/search"))
  println(totoDirectives("/search/about/somehting"))
  println(totoDirectives("/toto"))
}