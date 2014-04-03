package org.gszeliga.potok.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.input.CharArrayReader
import java.io.BufferedInputStream
import java.io.FileInputStream
import scala.io.Source
import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader

@RunWith(classOf[JUnitRunner])
class TestBencode extends FlatSpec with Matchers {

  behavior of "Bencode parser"

  it must "parse a space" in {
    val str = new CharArrayReader("2:  ".toCharArray())
    val result = Bencode.parse(str)

    result should be(Left(bBString("  ")))
  }

  it must "parse an url" in {
    val str = new CharArrayReader("36:udp://tracker.openbittorrent.com:80/".toCharArray())
    val result = Bencode.parse(str)

    result should be(Left(bBString("udp://tracker.openbittorrent.com:80/")))
  }

  it must "parse a comment" in {
    val str = new CharArrayReader("56:Visit #EZTV on EFNet (irc.efnet.info) or http://eztv.it/".toCharArray())
    val result = Bencode.parse(str)

    result should be(Left(bBString("Visit #EZTV on EFNet (irc.efnet.info) or http://eztv.it/")))
  }

  it must "parse a positive integer" in {
    val int = new CharArrayReader("i350e".toCharArray())
    val result = Bencode.parse(int)

    result should be(Left(bBInt(350)))
  }

  it must "parse a negative integer" in {
    val int = new CharArrayReader("i-63e".toCharArray())
    val result = Bencode.parse(int)

    result should be(Left(bBInt(-63)))
  }

  it must "parse a list of strings" in {
    val list = new CharArrayReader("l4:spam4:eggse".toCharArray())
    val result = Bencode.parse(list)

    result should be(Left(bBList(List(bBString("spam"), bBString("eggs")))))
  }

  it must "parse a list of integers" in {
    val list = new CharArrayReader("li89ei-7ee".toCharArray())
    val result = Bencode.parse(list)

    result should be(Left(bBList(List(bBInt(89), bBInt(-7)))))
  }

  it must "parse a single entry dictionary" in {
    val dict = new CharArrayReader("d9:publisher3:bobe".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Left(bBDict(Map(bBString("publisher") -> bBString("bob")))))
  }

  it must "parse an integer and string multiple entry dictionary" in {
    val dict = new CharArrayReader("d9:publisher3:bob3:agei45e17:publisher-webpage15:www.example.com18:publisher.location4:homee".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Left(bBDict(Map(bBString("publisher") -> bBString("bob"), bBString("age") -> bBInt(45), bBString("publisher-webpage") -> bBString("www.example.com"), bBString("publisher.location") -> bBString("home")))))
  }

  it must "parse a string-only multiple entry dictionary" in {
    val dict = new CharArrayReader("d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Left(bBDict(Map(bBString("publisher") -> bBString("bob"), bBString("publisher-webpage") -> bBString("www.example.com"), bBString("publisher.location") -> bBString("home")))))
  }

  it must "parse a list within a dictionary" in {
    val dict = new CharArrayReader("d5:namesl9:guillermo8:fernandoee".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Left(bBDict(Map(bBString("names") -> bBList(List(bBString("guillermo"), bBString("fernando")))))))
  }

  it must "parse a list and an integer within a dictionary" in {
    val dict = new CharArrayReader("d5:namesl9:guillermo8:fernandoe4:timei1234ee".toCharArray())
    val result = Bencode.parse(dict)

    result should be(Left(bBDict(Map(bBString("names") -> bBList(List(bBString("guillermo"), bBString("fernando"))), bBString("time") -> bBInt(1234)))))
  }

  it must "parse a list of lists of strings" in {
    val list = new CharArrayReader("ll35:udp://tracker.openbittorrent.com:80el29:udp://tracker.publicbt.com:80el26:udp://tracker.istole.it:80el25:udp://open.demonii.com:80el32:udp://tracker.coppersurfer.tk:80el23:udp://tracker.ccc.de:80ee".toCharArray())
    val result = Bencode.parse(list)

    result should be(Left(bBList(List(bBList(List(bBString("udp://tracker.openbittorrent.com:80"))), bBList(List(bBString("udp://tracker.publicbt.com:80"))), bBList(List(bBString("udp://tracker.istole.it:80"))), bBList(List(bBString("udp://open.demonii.com:80"))), bBList(List(bBString("udp://tracker.coppersurfer.tk:80"))), bBList(List(bBString("udp://tracker.ccc.de:80")))))))
  }

  it must "use the right encoding" in {
    
    val file = getClass().getResource("/samples/torrent/The.Walking.Dead.S04E01.HDTV.x264-ASAP.eztv.torrent").getFile()
    
    //479
    val bis = new BufferedInputStream(new FileInputStream(file))
    val rawBytes = Stream.continually(bis.read).drop(478).takeWhile(-1 !=).map(_.toHexString)
 
    val stream = StreamReader(new InputStreamReader(new FileInputStream(file), "ISO-8859-15"))
    
    def read(s: StreamReader, r: String): String = {
      if(s.atEnd) r
      else {
        read(s.rest, r + "|" + s.first.toHexString)
      }
    }
    
    val fromRawBytes = rawBytes.foldLeft("")(_ + "|" + _)
    val fromEncodedBytes = read(stream.drop(478), "")
    
    //println(fromRawBytes.foldLeft("")(_ + "|" + _).length / 2) //21722
    //println(fromEncodedBytes.length / 2)

    println(fromRawBytes.split("|").size)
    println(fromEncodedBytes.split("|").size)
    
    //fromRawBytes should be(fromEncodedBytes) 
    
  }

  ignore must "parse a whole file" in {
    //val result = Bencode.parse(getClass().getResource("/samples/torrent/The.Walking.Dead.S04E01.HDTV.x264-ASAP.eztv.torrent"))
    val result = Bencode.parse(getClass().getResource("/samples/torrent/enciclopedia-de-la-astronomia-y-el28.torrent"))

    result should be(Left)

  }
}