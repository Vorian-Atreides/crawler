package io.musubu.crawler.robot

import cats.implicits._
import cats.{ Applicative, Monoid }

private object Lexical {
  sealed trait Token

  sealed trait UserAgent extends Token
  final case class AnyAgent() extends UserAgent
  final case class SpecificAgent(name: String) extends UserAgent

  sealed trait Directive extends Token {
    val path: String
  }
  final case class Allow(path: String) extends Directive
  final case class Disallow(path: String) extends Directive
  final case class DefaultDirective() extends Directive {
    val path = "/"
  }

  final case class LexicalParserResult[F[_]](errors: F[String], tokens: F[Token])
}

private object LexicalParser {
  import Lexical._

  private def userAgent(name: String): Token = name match {
    case "*" => AnyAgent()
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
    case _ => Left(s"unsupported directive '${key}'")
  }

  private def sanitize(value: String): String = {
    val str = value.indexOf('#') match {
      case -1 => value
      case other => value.take(other)
    }
    str.filterNot(_.isWhitespace)
  }

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

  def parse[F[_]](data: String)(implicit ms: Monoid[F[String]], mt: Monoid[F[Token]], app: Applicative[F]): LexicalParserResult[F] =
    data.split("\n").
      map(_.filterNot(_.isWhitespace)). // Remove the whitespaces.
      toList.zipWithIndex.
      filterNot { case (value, _) => isIgnoredLine(value) }. // Remove the empty lines and the comments.
      map { case (value, i) => // Extends the error message with the error line.
        parseLine(value).leftMap(error => s"Line ${i+1}: ${error}")
      }.foldRight(LexicalParserResult(ms.empty, mt.empty)){ (result, acc) =>
      result match {
        case Left(error) => acc.copy(errors = ms.combine(app.pure(error), acc.errors))
        case Right(token) => acc.copy(tokens = mt.combine(app.pure(token), acc.tokens))
      }
    }

}
