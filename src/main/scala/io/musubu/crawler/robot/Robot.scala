package io.musubu.crawler.robot

import io.lemonlabs.uri._
import cats.implicits._

import Lexical._

object Robot {
  def apply(data: String): Robot = {
    val parsingResult = LexicalParser.parse[List](data)
    val groups = SyntacticParser.compile(parsingResult.tokens)
    new Robot(groups)
  }
}

class Robot private (groups: Map[UserAgent, List[Directive]]) {

  private def longestMatchingUserAgent(ua: String): UserAgent =
    LazyList.range(ua.length, 0, -1).
      map(n => SpecificAgent(ua.take(n))).
      find{ ua =>
        groups.contains(ua)
      }.getOrElse(AnyAgent())

  private def longestMatchingPath(ua: UserAgent)(path: String): Boolean = {
    val uri = Uri.parse(path)
    groups.get(ua).flatMap { directives =>
      directives.find { directive =>
        uri.path == Path.parse(directive.path)
      }
    } map {
      case DefaultDirective() => true
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
