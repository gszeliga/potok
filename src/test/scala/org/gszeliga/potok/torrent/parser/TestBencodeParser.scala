package org.gszeliga.potok.torrent.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.combinator.Parsers
import scala.util.Left

@RunWith(classOf[JUnitRunner])
class TestBencodeParser extends FlatSpec with Matchers {

  behavior of "Bencode parser"

  it must "parse an US-ASCII single digit" in {
    val digit = new ByteReader("3".getBytes("US-ASCII"))
    val result = Bencode.single_digit(digit).getOrElse("NaN")

    result should be("3")
  }

  it must "parse an US-ASCII natural number" in {
    val number = new ByteReader("480".getBytes("US-ASCII"))
    val result = Bencode.natural(number).getOrElse("NaN")

    result should be(480)
  }

  it must "parse an US-ASCII positive number" in {
    val number = new ByteReader("665".getBytes("US-ASCII"))
    val result = Bencode.signedInt(number).getOrElse("NaN")

    result should be(665)
  }

  it must "parse an US-ASCII negative number" in {
    val number = new ByteReader("-1562".getBytes("US-ASCII"))
    val result = Bencode.signedInt(number).getOrElse("NaN")

    result should be(-1562)
  }

  it must "parse a simple string" in {
    val string = new ByteReader("4:hola".getBytes("UTF-8"))
    val result = Bencode.string(string).getOrElse(BString(Nil))

    result.create("UTF-8") should be("hola")
  }

  it must "parse a space" in {
    val str = new ByteReader("2:  ".getBytes("UTF-8"))
    val result = Bencode.string(str).getOrElse(BString(Nil))

    result.create("UTF-8") should be("  ")
  }

  it must "parse an url" in {
    val str = new ByteReader("36:udp://tracker.openbittorrent.com:80/".getBytes("UTF-8"))
    val result = Bencode.string(str).getOrElse(BString(Nil))

    result.create("UTF-8") should be("udp://tracker.openbittorrent.com:80/")
  }

  it must "parse a comment" in {
    val str = new ByteReader("56:Visit #EZTV on EFNet (irc.efnet.info) or http://eztv.it/".getBytes("UTF-8"))
    val result = Bencode.string(str).getOrElse(BString(Nil))

    result.create("UTF-8") should be("Visit #EZTV on EFNet (irc.efnet.info) or http://eztv.it/")
  }

  it must "parse a positive integer" in {
    val int = new ByteReader("i350e".getBytes())
    val result = Bencode.int(int).getOrElse(BInt(0)).get

    result should be(350)
  }

  it must "parse a negative integer" in {
    val int = new ByteReader("i-63e".getBytes())
    val result = Bencode.int(int).getOrElse(BInt(0)).get

    result should be(-63)
  }
  it must "parse a list of strings" in {
    val list = new ByteReader("l4:spam4:eggse".getBytes("US-ASCII"))
    val result = Bencode.list(list).getOrElse(BList(Nil))

    result should be(BList(List(BString("spam".getBytes("US-ASCII").toList), BString("eggs".getBytes("US-ASCII").toList))))
  }

  it must "parse a list of integers" in {
    val list = new ByteReader("li89ei-7ee".getBytes("US-ASCII"))
    val result = Bencode.list(list).getOrElse(BList(Nil))

    result should be(BList(List(BInt(89), BInt(-7))))
  }

  it must "parse a single entry dictionary" in {
    val dict = new ByteReader("d9:publisher3:bobe".getBytes("US-ASCII"))
    val result = Bencode.dict(dict).getOrElse(BDict(Map.empty))

    result should be(BDict(Map(BString("publisher".getBytes("US-ASCII").toList) -> BString("bob".getBytes("US-ASCII").toList))))
  }

  it must "parse an integer and string multiple entry dictionary" in {
    val dict = new ByteReader("d9:publisher3:bob3:agei45e17:publisher-webpage15:www.example.com18:publisher.location4:homee".getBytes("US-ASCII"))
    val result = Bencode.dict(dict).getOrElse(BDict(Map.empty))

    result should be(BDict(Map(BString("publisher".getBytes("US-ASCII").toList) -> BString("bob".getBytes("US-ASCII").toList), BString("age".getBytes("US-ASCII").toList) -> BInt(45), BString("publisher-webpage".getBytes("US-ASCII").toList) -> BString("www.example.com".getBytes("US-ASCII").toList), BString("publisher.location".getBytes("US-ASCII").toList) -> BString("home".getBytes("US-ASCII").toList))))
  }

  it must "parse a string-only multiple entry dictionary" in {
    val dict = new ByteReader("d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee".getBytes("US-ASCII"))
    val result = Bencode.dict(dict).getOrElse(BDict(Map.empty))

    result should be(BDict(Map(BString("publisher".getBytes("US-ASCII").toList) -> BString("bob".getBytes("US-ASCII").toList), BString("publisher-webpage".getBytes("US-ASCII").toList) -> BString("www.example.com".getBytes("US-ASCII").toList), BString("publisher.location".getBytes("US-ASCII").toList) -> BString("home".getBytes("US-ASCII").toList))))
  }

  it must "parse a list within a dictionary" in {
    val dict = new ByteReader("d5:namesl9:guillermo8:fernandoee".getBytes("US-ASCII"))
    val result = Bencode.dict(dict).getOrElse(BDict(Map.empty))

    result should be(BDict(Map(BString("names".getBytes("US-ASCII").toList) -> BList(List(BString("guillermo".getBytes("US-ASCII").toList), BString("fernando".getBytes("US-ASCII").toList))))))
  }

  it must "parse a list and an integer within a dictionary" in {
    val dict = new ByteReader("d5:namesl9:guillermo8:fernandoe4:timei1234ee".getBytes("US-ASCII"))
    val result = Bencode.dict(dict).getOrElse(BDict(Map.empty))

    result should be(BDict(Map(BString("names".getBytes("US-ASCII").toList) -> BList(List(BString("guillermo".getBytes("US-ASCII").toList), BString("fernando".getBytes("US-ASCII").toList))), BString("time".getBytes("US-ASCII").toList) -> BInt(1234))))
  }

  it must "parse a list of lists of strings" in {
    val list = new ByteReader("ll35:udp://tracker.openbittorrent.com:80el29:udp://tracker.publicbt.com:80el26:udp://tracker.istole.it:80el25:udp://open.demonii.com:80el32:udp://tracker.coppersurfer.tk:80el23:udp://tracker.ccc.de:80ee".getBytes("UTF-8"))
    val result = Bencode.list(list).getOrElse(BList(Nil))

    result should be(BList(List(BList(List(BString("udp://tracker.openbittorrent.com:80".getBytes("US-ASCII").toList))), BList(List(BString("udp://tracker.publicbt.com:80".getBytes("US-ASCII").toList))), BList(List(BString("udp://tracker.istole.it:80".getBytes("US-ASCII").toList))), BList(List(BString("udp://open.demonii.com:80".getBytes("US-ASCII").toList))), BList(List(BString("udp://tracker.coppersurfer.tk:80".getBytes("US-ASCII").toList))), BList(List(BString("udp://tracker.ccc.de:80".getBytes("US-ASCII").toList))))))
  }
  
  it must "parse a whole file with english strings" in {
    val result = Bencode.parse(getClass().getResource("/samples/torrent/The.Walking.Dead.S04E01.HDTV.x264-ASAP.eztv.torrent"))
    result.isLeft should be(true)
  }  

  it must "parse a whole file with spanish strings" in {
    val result = Bencode.parse(getClass().getResource("/samples/torrent/enciclopedia-de-la-astronomia-y-el28.torrent"))
    result.isLeft should be (true)
  }
  
}