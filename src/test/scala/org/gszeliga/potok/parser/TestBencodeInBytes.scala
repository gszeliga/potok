package org.gszeliga.potok.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.combinator.Parsers

@RunWith(classOf[JUnitRunner])
class TestBencodeInBytes extends FlatSpec with Matchers {

  behavior of "Bencode parser"

  it must "parse an US-ASCII single digit" in {
    val digit = new ByteReader("3".getBytes("US-ASCII"))
    val result = BencodeInBytes.single_digit(digit).getOrElse("NaN")

    result should be("3")
  }

  it must "parse an US-ASCII natural number" in {
    val number = new ByteReader("480".getBytes("US-ASCII"))
    val result = BencodeInBytes.natural(number).getOrElse("NaN")

    result should be(480)
  }

  it must "parse an US-ASCII positive number" in {
    val number = new ByteReader("665".getBytes("US-ASCII"))
    val result = BencodeInBytes.signedInt(number).getOrElse("NaN")

    result should be(665)
  }  

  it must "parse an US-ASCII negative number" in {
    val number = new ByteReader("-1562".getBytes("US-ASCII"))
    val result = BencodeInBytes.signedInt(number).getOrElse("NaN")

    result should be(-1562)
  }    
  
}