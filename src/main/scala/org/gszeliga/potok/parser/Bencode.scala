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

trait BencodeType

case class BString(get: String) extends BencodeType
case class BInt(get: Int) extends BencodeType
case class BList(get: List[BencodeType]) extends BencodeType
case class BDict(get: Map[BString, BencodeType]) extends BencodeType

class Parser extends RegexParsers {

  implicit def fromCharToParser(ch: Char) = elem(ch)

  //holy fuck.....this was a headache to find
  override def skipWhitespace = false

  def natural: Parser[Int] = """\d+""".r ^^ (_.toInt) named ("natural")
  def signedInt: Parser[Int] = """(-){0,1}\d+""".r ^^ (_.toInt) named ("signed_int")

  //http://sanjaal.com/java/tag/java-regular-expression-posix-character-classes-us-ascii-only/
  //www.regexplanet.com/advanced/java/index.html
  def literal: Parser[String] = """[\p{Print}]""".r named ("printable characters")

  def byteString = new Parser[String] {
    def apply(in: Input) = {
      if (in.atEnd) Failure("end of input", in)
      else Success(in.first.toChar.toHexString, in.rest)
    }
  } named ("byteString")

  def delimitedBy[A](left: Parser[Char], right: Parser[Char])(p: Parser[A]): Parser[A] = left ~> p <~ right

  def string: Parser[BString] =
    natural ~ ':' >> {
      case size ~ _ => repN(size, literal) ^^ (l => BString(l.mkString))
    } named ("string")

  def bstring: Parser[BString] =
    natural ~ ':' >> {
      case size ~ _ => repN(size, byteString) ^^ (l => BString(l.mkString))
    } named ("bstring")    
    
  def int: Parser[BInt] = {
    delimitedBy('i', 'e')(signedInt) ^^ (BInt(_)) named ("int")
  }

  def strings = string | bstring
    
  def list: Parser[BList] = {
    delimitedBy('l', 'e') {
      rep(strings | int | list | dict)
    } ^^ (BList(_)) named ("list")
  }

  def dict: Parser[BDict] = {
    delimitedBy('d', 'e') {
      rep(string ~ (strings | int | list | dict))
    } ^^ (_.map { case key ~ value => key -> value }) ^^ (l => BDict(l.toMap)) named ("dict")
  }

  def root = strings | int | list | dict

}

object Bencode extends Parser {

  class ParseError(val msg: String, val next: Input) {
    override def toString = s"'$msg' at ${next.pos}"
  }

  def parse(in: Reader[Char]): Either[BencodeType, ParseError] = phrase(root)(in) match {
    case Success(r, _) => Left(r)
    case NoSuccess(msg, next) => Right(new ParseError(msg, next))
  }

  def parse(in: URL): Either[BencodeType, ParseError] = parse(StreamReader(new InputStreamReader(new FileInputStream(in.getFile()), "US-ASCII")))
}