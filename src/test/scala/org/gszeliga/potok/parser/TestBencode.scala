package org.gszeliga.potok.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.input.CharArrayReader

@RunWith(classOf[JUnitRunner])
class TestBencode extends FlatSpec with Matchers {

  "Bencode" must "parse a string" in {
    val str = new CharArrayReader("36:udp://tracker.openbittorrent.com:80/".toCharArray())
    val result = Bencode.parse(str)

    result should be(Some(BString("udp://tracker.openbittorrent.com:80/")))
  }

  it must "parse a positive integer" in {
    val int = new CharArrayReader("i350e".toCharArray())
    val result = Bencode.parse(int)

    result should be(Some(BInt(350)))
  }

  it must "parse a negative integer" in {
    val int = new CharArrayReader("i-63e".toCharArray())
    val result = Bencode.parse(int)

    result should be(Some(BInt(-63)))
  }

  it must "parse a list of strings" in {
    val list = new CharArrayReader("l4:spam4:eggse".toCharArray())
    val result = Bencode.parse(list)

    result should be(Some(BList(List(BString("spam"), BString("eggs")))))
  }

  it must "parse a list of integers" in {
    val list = new CharArrayReader("li89ei-7ee".toCharArray())
    val result = Bencode.parse(list)

    result should be(Some(BList(List(BInt(89), BInt(-7)))))
  }

  it must "parse a single entry dictionary" in {
    val dict = new CharArrayReader("d9:publisher3:bobe".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Some(BDict(Map(BString("publisher") -> BString("bob")))))
  }

  it must "parse an integer and string multiple entry dictionary" in {
    val dict = new CharArrayReader("d9:publisher3:bob3:agei45e17:publisher-webpage15:www.example.com18:publisher.location4:homee".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Some(BDict(Map(BString("publisher") -> BString("bob"), BString("age") -> BInt(45), BString("publisher-webpage") -> BString("www.example.com"), BString("publisher.location") -> BString("home")))))
  }

  it must "parse a string-only multiple entry dictionary" in {
    val dict = new CharArrayReader("d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Some(BDict(Map(BString("publisher") -> BString("bob"), BString("publisher-webpage") -> BString("www.example.com"), BString("publisher.location") -> BString("home")))))
  }

  it must "parse a list within a dictionary" in {
    val dict = new CharArrayReader("d5:namesl9:guillermo8:fernandoee".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Some(BDict(Map(BString("names") -> BList(List(BString("guillermo"), BString("fernando")))))))
  }

  it must "parse a list and an integer within a dictionary" in {
    val dict = new CharArrayReader("d5:namesl9:guillermo8:fernandoe4:timei1234ee".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Some(BDict(Map(BString("names") -> BList(List(BString("guillermo"), BString("fernando"))), BString("time") -> BInt(1234)))))
  }  
  
}