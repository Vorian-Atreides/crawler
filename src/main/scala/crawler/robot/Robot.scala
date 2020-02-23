package crawler.robot

import io.lemonlabs.uri._

object Robot {
  def fromString(data: String): Robot = {
    val parsingResult = LexicalParser.parse(data)
    val groups = SyntacticParser.compile(parsingResult.tokens)
    Robot(groups)
  }
}

case class Robot(groups: Map[UserAgent, List[Directive]]) {
  private def longestMatchingUserAgent(ua: String): UserAgent =
    LazyList.range(ua.length, 0, -1).
      map(n => SpecificAgent(ua.take(n))).
      find{ ua =>
        groups.contains(ua)
      }.getOrElse(AnyAgent)

  private def longestMatchingPath(ua: UserAgent)(path: String): Boolean = {
    val uri = Uri.parse(path)
    groups.get(ua).flatMap { directives =>
      directives.find { directive =>
        uri.path == Path.parse(directive.path)
      }
    }.map {
      case Allow(_) => true
      case Disallow(_) => false
    } getOrElse (true)
  }

  def isAllowed(userAgent: String): String => Boolean = {
    // Iterate through the given User-Agent to find the longest matching group.
    val ua = longestMatchingUserAgent(userAgent)
    longestMatchingPath(ua)
  }
}
