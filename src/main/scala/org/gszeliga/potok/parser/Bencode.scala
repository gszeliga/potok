package org.gszeliga.potok.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader
import scala.util.parsing.combinator._

trait BencodeType

case class BString(get: String) extends BencodeType
case class BInt(get: Int) extends BencodeType
case class BList(get: List[BencodeType]) extends BencodeType
case class BDict(get: Map[BString, BencodeType]) extends BencodeType

class Parser extends RegexParsers {

  def colon: Parser[Char] = elem(':') named ("colon")
  def natural: Parser[Int] = """\d+""".r ^^ (_.toInt) named ("natural")
  def signedInt: Parser[Int] = """(-){0,1}\d+""".r ^^ (_.toInt) named ("signed_int")
  def literal: Parser[String] = """[\w\W\:\/\.]""".r named ("literal")

  def string: Parser[BString] =
    natural ~ colon >> {
      case size ~ c => repN(size, literal) ^^ (l => BString(l.mkString))
    } named ("string")

  def int: Parser[BInt] = {
    elem('i') ~> signedInt <~ elem('e') ^^ (BInt(_)) named ("int")
  }

  def list: Parser[BList] = {
    elem('l') ~> rep(string | int | list | dict) <~ elem('e') ^^ (BList(_)) named ("list")
  }

  def dict: Parser[BDict] = {
    elem('d') ~> rep(string ~ (string | int | list | dict)) <~ elem('e') ^^ (_.map { case a ~ b => a -> b }) ^^ (l => BDict(l.toMap)) named ("dict")
  }

  def root = string | int | list | dict

}

object Bencode extends Parser {
  def parse(in: Reader[Char]): Option[BencodeType] = phrase(root)(in) match {
    case Success(r, _) => Some(r)
    case _ => None
  }
}