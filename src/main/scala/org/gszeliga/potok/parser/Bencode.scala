package org.gszeliga.potok.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader
import scala.util.parsing.combinator._

trait BencodeType

case class BString(get: String) extends BencodeType
case class BInt extends BencodeType
case class BList extends BencodeType
case class BDict extends BencodeType

class Parser extends RegexParsers{

  def colon: Parser[Char] = elem(':') named ("colon")
  def digits: Parser[Int] = """\d+""".r ^^ (_.toInt) named ("digits")
  def literal: Parser[String] = """[\w\:\/\.]""".r named ("literal")

  def string: Parser[BString] =
    digits ~ colon >> {
      s => repN(s._1, literal) ^^ (l => BString(l.mkString))
    } named ("string")

  def root = string

}

object Bencode extends Parser {
  def parse(in: Reader[Char]): Option[BencodeType] = phrase(root)(in) match {
    case Success(r, _) => Some(r)
    case _ => None
  }
}