package org.gszeliga.potok.torrent.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.net.URL

import org.gszeliga.potok.torrent.parser.ByteReader;

sealed trait BencodeType

object BEmpty extends BencodeType

case class BString(get: List[Byte]) extends BencodeType {

  def create(enc: String) = new String(get.toArray, enc)

  override def toString = {
    //We use a 1-byte encoding since it's compliant with specification
    val output = new String(get.take(200).toArray, "ISO-8859-15")
    if (get.length > 200) output + " ..." else output
  }
}

case class BInt(get: Int) extends BencodeType {
  override def toString = get.toString
}

case class BList(get: List[BencodeType]) extends BencodeType {
  def flatten = {
    def doFlat(l: List[BencodeType]): List[BencodeType] = {
      l match {
        case BList(l) :: tail => doFlat(l) ++ doFlat(tail)
        case head :: tail => head :: doFlat(tail)
        case _ => Nil
      }
    }
    doFlat(get)
  }
}

case class BDict(get: Map[BString, BencodeType]) extends BencodeType

trait BencodeParser extends Parsers {

  type Elem = Byte

  implicit def charToParser(ch: Char) = elem(ch.toByte)

  def delimitedBy[A](left: Parser[Byte], right: Parser[Byte])(p: Parser[A]): Parser[A] = left ~> p <~ right

  def single_digit = new Parser[String] {

    val re = """\d""".r

    def apply(in: Input) = {
      if (in.atEnd) Failure("End of input reached", in)
      else {
        //Specification says numbers are coded in ASCII
        val n = new String(Array(in.first.asInstanceOf[Byte]), "US-ASCII")
        re.findFirstIn(n) map (Success(_, in.rest)) getOrElse (Failure(s"'$n' is not a number", in))
      }
    }
  } named ("digit")

  def natural = rep1(single_digit) ^^ (_.mkString.toInt) named ("natural")

  def signedInt = new Parser[Int] {
    def apply(in: Input) = {
      '-'(in) match {
        case Success(_, rest) => natural(rest) map (_ * -1)
        case _ => natural(in)
      }
    }
  } named ("signed_int")

  def rawbyte = new Parser[Byte] {
    def apply(in: Input) = {
      if (in.atEnd) Failure("End of input reached", in)
      else Success(in.first, in.rest)
    }
  } named ("raw_bytes")

  //After different tests performed, I arrived to the conclusion that size = bytes
  def string: Parser[BString] =
    natural ~ ':' >> {
      case size ~ _ => repN(size, rawbyte) ^^ (BString(_))
    } named ("string")

  def int: Parser[BInt] = {
    delimitedBy('i', 'e')(signedInt) ^^ (BInt(_)) named ("int")
  }

  def list: Parser[BList] = {
    delimitedBy('l', 'e') {
      rep(string | int | list | dict)
    } ^^ (BList(_)) named ("list")
  }

  def dict: Parser[BDict] = {
    delimitedBy('d', 'e') {
      rep(string ~ (string | int | list | dict))
    } ^^ (_.map { case key ~ value => key -> value }) ^^ (l => BDict(l.toMap)) named ("dict")
  }

  def root = string | int | list | dict

}

object Bencode extends BencodeParser {

  class ParseError(val msg: String, val next: Input) {
    override def toString = s"'$msg' at ${next.pos}"
  }

  def parse(in: Reader[Byte]): Either[BencodeType, ParseError] = phrase(root)(in) match {
    case Success(r, _) => Left(r)
    case NoSuccess(msg, next) => Right(new ParseError(msg, next))
  }

  def parse(from: URL): Either[BencodeType, ParseError] = {

    val bis = new BufferedInputStream(new FileInputStream(from.getFile))
    val bytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    parse(new ByteReader(bytes))
  }
}