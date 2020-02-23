package crawler.robot

import cats.implicits._

import scala.util.{Try, Success, Failure}

trait Token

trait UserAgent extends Token
case object AnyAgent extends UserAgent
case class SpecificAgent(name: String) extends UserAgent

trait Directive extends Token {
  val path: String
}
case class Allow(path: String) extends Directive
case class Disallow(path: String) extends Directive
object DefaultDirective extends Allow("/")

case class LexicalParserResult(errors: List[String], tokens: List[Token])

object LexicalParser {
  private def userAgent(name: String): Token = name match {
    case "*" => AnyAgent
    case name => SpecificAgent(name)
  }

  private def allow(path: String): Token =
    Allow(path)

  private def disallow(path: String): Token =
    Disallow(path)

  private def token(key: String, value: String): Either[String, Token] = key.toLowerCase match {
    case "user-agent" => Right(userAgent(value))
    case "allow" => Right(allow(value))
    case "disallow" => Right(disallow(value))
    // Tolerate the unrecognized tokens.
    case _ => Left(s"unsupported directive '${key}'")
  }

  private def sanitize(value: String): String = {
    val str = value.indexOf('#') match {
      case -1 => value
      case other => value.take(other)
    }
    str.filterNot(_.isWhitespace)
  }

  private def split(line: String): Try[List[String]] =
    Try(line.split(":").toList)

  private def parseLine(line: String): Either[String, Token] = line.indexOf(":") match {
    case -1 => Left(s"missing character ':'")
    case other if other + 1 < line.length =>
      val key = line.take(other)
      val value = line.substring(other+1)
      token(key, sanitize(value))
    case _ => Left("the given field has no associated value")
  }

  private def isIgnoredLine(line: String): Boolean =
    line == "" || line.startsWith("#")

  def parse(data: String): LexicalParserResult = {
    val results = data.split("\n").
      map(_.filterNot(_.isWhitespace)).
      toList.zipWithIndex.
      filterNot { case (value, _) => isIgnoredLine(value)}.
      map { case (value, i) =>
        parseLine(value).leftMap(error => s"Line ${i+1}: ${error}")
      }
    val errors = results.filter(_.isLeft).map(_.left.get)
    val tokens = results.filter(_.isRight).map(_.right.get)
    LexicalParserResult(errors, tokens)
  }
}
