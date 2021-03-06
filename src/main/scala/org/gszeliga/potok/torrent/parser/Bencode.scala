package org.gszeliga.potok.torrent.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.net.URL

trait BencodeConstants {
  final val DEFAULT_STRING_ENCODING = "ISO-8859-15"
  final val DEFAULT_NUMBER_ENCODING = "US-ASCII"

  final val NUMBER_BEGIN: Char = 'i'
  final val NUMBER_END: Char = 'e'

  final val LIST_BEGIN = 'l'
  final val LIST_END = 'e'

  final val DICT_BEGIN = 'd'
  final val DICT_END = 'e'
}

sealed trait BencodeType extends BencodeConstants {
  self =>

  def unfold: Array[Byte] = {

    def within[V](begin: Char, end: Char)(v: V)(f: V => Array[Byte]) = {
      begin.toByte +: f(v) :+ end.toByte
    }

    self match {
      case BString(bytes) => (bytes.size.toString.getBytes(DEFAULT_NUMBER_ENCODING) :+ ':'.toByte) ++ bytes
      case BInt(int) => within(NUMBER_BEGIN, NUMBER_END)(int)(_.toString.getBytes(DEFAULT_NUMBER_ENCODING))
      case BList(l) => within(LIST_BEGIN, LIST_END)(l)(_.map(_.unfold).reduce(_ ++ _))
      case BDict(m) => within(DICT_BEGIN, DICT_END)(m)(_.map { case (k, v) => k.unfold ++ v.unfold } reduce(_ ++ _))
    }
  }
}

case class BString(get: List[Byte]) extends BencodeType {

  def create(enc: String = DEFAULT_STRING_ENCODING) = new String(get.toArray, enc)

  override def toString = {
    //We use a 1-byte encoding since it's compliant with specification
    val output = new String(get.take(200).toArray, DEFAULT_STRING_ENCODING)
    if (get.length > 200) output + " ..." else output
  }
}

case class BInt(get: Int) extends BencodeType {
  override def toString = get.toString
}

case class BList(get: List[BencodeType]) extends BencodeType

case class BDict(get: Map[BString, BencodeType]) extends BencodeType

trait BencodeParser extends Parsers with BencodeConstants {

  type Elem = Byte

  implicit def charToParser(ch: Char) = elem(ch.toByte)

  def delimitedBy[A](left: Parser[Byte], right: Parser[Byte])(p: Parser[A]): Parser[A] = left ~> p <~ right

  def single_digit = new Parser[String] {

    val re = """\d""".r

    def apply(in: Input) = {
      if (in.atEnd) Failure("End of input reached", in)
      else {
        //Specification says numbers are coded in ASCII
        val n = new String(Array(in.first.asInstanceOf[Byte]), DEFAULT_NUMBER_ENCODING)
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
    delimitedBy(NUMBER_BEGIN, NUMBER_END)(signedInt) ^^ (BInt(_)) named ("int")
  }

  def list: Parser[BList] = {
    delimitedBy(LIST_BEGIN, LIST_END) {
      rep(string | int | list | dict)
    } ^^ (BList(_)) named ("list")
  }

  def dict: Parser[BDict] = {
    delimitedBy(DICT_BEGIN, DICT_END) {
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