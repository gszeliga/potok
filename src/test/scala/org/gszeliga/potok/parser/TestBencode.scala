package org.gszeliga.potok.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.input.CharArrayReader

@RunWith(classOf[JUnitRunner])
class TestBencode extends FlatSpec with Matchers {

  "Bencode" must "extract a string type" in {

    val str = new CharArrayReader("36:udp://tracker.openbittorrent.com:80/".toCharArray())
    val result = Bencode.parse(str)

    result should be(Some(BString("udp://tracker.openbittorrent.com:80/")))

  }
}