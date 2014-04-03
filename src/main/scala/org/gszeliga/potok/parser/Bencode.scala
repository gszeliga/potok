package org.gszeliga.potok.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader
import scala.util.parsing.combinator._
import java.net.URL
import scala.io.Source
import java.io.FileReader
import java.io.InputStreamReader
import scala.util.parsing.input.StreamReader
import java.io.FileInputStream

trait bBencodeType

case class bBString(get: String) extends bBencodeType
case class bBInt(get: Int) extends bBencodeType
case class bBList(get: List[bBencodeType]) extends bBencodeType
case class bBDict(get: Map[bBString, bBencodeType]) extends bBencodeType

class Parser extends RegexParsers {

  implicit def fromCharToParser(ch: Char) = elem(ch)

  //holy fuck.....this was a headache to find
  override def skipWhitespace = false

  def natural: Parser[Int] = """\d+""".r ^^ (_.toInt) named ("natural")
  def signedInt: Parser[Int] = """(-){0,1}\d+""".r ^^ (_.toInt) named ("signed_int")

  //http://sanjaal.com/java/tag/java-regular-expression-posix-character-classes-us-ascii-only/
  //www.regexplanet.com/advanced/java/index.html
  def literal: Parser[String] = """[\p{L}\p{M}\p{N}\p{Z}\p{S}\p{S}\p{P}]""".r named ("printable characters")

  def byteString = new Parser[String] {
    def apply(in: Input) = {
      if (in.atEnd) Failure("end of input", in)
      else Success(in.first.toChar.toHexString, in.rest)
    }
  } named ("byteString")

  def delimitedBy[A](left: Parser[Char], right: Parser[Char])(p: Parser[A]): Parser[A] = left ~> p <~ right

  def string: Parser[bBString] =
    natural ~ ':' >> {
      case size ~ _ => repN(size, literal) ^^ (l => bBString(l.mkString))
    } named ("string")

  def bstring: Parser[bBString] =
    natural ~ ':' >> {
      case size ~ _ => repN(size, byteString) ^^ (l => bBString(l.mkString))
    } named ("bstring")    
    
  def int: Parser[bBInt] = {
    delimitedBy('i', 'e')(signedInt) ^^ (bBInt(_)) named ("int")
  }

  def strings = string | bstring
    
  def list: Parser[bBList] = {
    delimitedBy('l', 'e') {
      rep(strings | int | list | dict)
    } ^^ (bBList(_)) named ("list")
  }

  def dict: Parser[bBDict] = {
    delimitedBy('d', 'e') {
      rep(string ~ (strings | int | list | dict))
    } ^^ (_.map { case key ~ value => key -> value }) ^^ (l => bBDict(l.toMap)) named ("dict")
  }

  def root = strings | int | list | dict

}

object Bencode extends Parser {

  class ParseError(val msg: String, val next: Input) {
    override def toString = s"'$msg' at ${next.pos}"
  }

  def parse(in: Reader[Char]): Either[bBencodeType, ParseError] = phrase(root)(in) match {
    case Success(r, _) => Left(r)
    case NoSuccess(msg, next) => Right(new ParseError(msg, next))
  }

  def parse(in: URL): Either[bBencodeType, ParseError] = parse(StreamReader(new InputStreamReader(new FileInputStream(in.getFile()), "ISO-8859-15")))
}