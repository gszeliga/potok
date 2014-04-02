package org.gszeliga.potok.parser

import scala.util.parsing.combinator.Parsers

trait BencodeInBytes extends Parsers {

  type Elem = Byte

  def single_digit = new Parser[String] {

    val re = """\d""".r

    def apply(in: Input) = {
      val n = new String(Array(in.first.asInstanceOf[Byte]), "US-ASCII")
      re.findFirstIn(n) map (Success(_, in.rest)) getOrElse (Failure(s"'$n' is not a number", in.rest))
    }
  }

  def natural = rep(single_digit) ^^ (_.mkString.toInt)
  
  def signedInt = new Parser[Int] {
    def apply(in: Input) = {
      elem('-'.toByte)(in) match {
        case Success(_, rest) => natural(rest) map (_ * -1)
        case _ => natural(in)
      }
    }
  }

}

object BencodeInBytes extends BencodeInBytes {

}
