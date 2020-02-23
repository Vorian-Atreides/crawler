package crawler.robot

import scala.collection.immutable.HashMap

object SyntacticParser {
  trait Group
  case class UserAgentLeaf(userAgent: UserAgent, next: DirectiveGroup) extends Group

  trait DirectiveGroup extends Group {
    def toList: List[Directive]
  }

  case object NoneLeaf extends DirectiveGroup {
    def toList: List[Directive] = List()
  }

  case class DirectiveLeaf(directive: Directive, next: DirectiveGroup) extends DirectiveGroup {
    def toList: List[Directive] = {
      def rec(current: DirectiveGroup, acc: List[Directive]): List[Directive] = current match {
        case NoneLeaf => acc
        case c: DirectiveLeaf => rec(c.next, c.directive :: acc)
      }
      rec(next, List(directive)).reverse
    }
  }

  private def userAgent(token: Token, acc: UserAgentLeaf): List[Group] = token match {
    case ua: UserAgent => List(UserAgentLeaf(ua, acc.next), acc)
    case d: Directive => List(DirectiveLeaf(d, NoneLeaf), acc)
  }

  private def directiveGroup(token: Token, acc: DirectiveGroup): List[Group] = token match {
    case ua: UserAgent => List(UserAgentLeaf(ua, acc))
    case d: Directive => List(DirectiveLeaf(d, acc))
  }

  private def none(token: Token, acc: DirectiveGroup): List[Group] = token match {
    case ua: UserAgent => List(UserAgentLeaf(ua, DirectiveLeaf(DefaultDirective, acc)))
    case d: Directive => List(DirectiveLeaf(d, acc))
  }

  private def parseToken(token: Token, acc: Group): List[Group] = acc match {
    case n: NoneLeaf.type => none(token, n)
    case d: DirectiveGroup => directiveGroup(token, d)
    case ua: UserAgentLeaf => userAgent(token, ua)
  }

  private def parseGroups(tokens: List[Token]): List[Group] = {
    val default: List[Group] = List(NoneLeaf)
    tokens.foldRight(default) { (token, acc) =>
      val group :: rest = acc
      val nextGroups = parseToken(token, group)
      nextGroups ::: rest
    }
  }

  def compile(tokens: List[Token]): Map[UserAgent, List[Directive]] = {
    // Filter out the groups without User-Agent.
    val groups = parseGroups(tokens) filter {
      case _: UserAgentLeaf => true
      case _ => false
    }

    groups.foldRight(new HashMap[UserAgent, List[Directive]]) { (group, acc) =>
      val UserAgentLeaf(ua, newDirectives) = group
      val directives = acc.get(ua) match {
        case Some(oldDirectives) => newDirectives.toList ::: oldDirectives
        case None => newDirectives.toList
      }
      acc + (ua -> directives)
    } map { case (ua, directives) =>
      val sortedDirectives = directives.sortBy(a => a.path.length)(Ordering.Int.reverse)
      (ua, sortedDirectives)
    }
  }
}
