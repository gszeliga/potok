package org.gszeliga.potok.torrent.parser

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestBencodeTypes extends FlatSpec with Matchers {

  behavior of "Bencode types"

  it must "be able to unfold a string type" in {
    val bytes = "11:thisisatest".getBytes("ISO-8859-15")
    val string = new ByteReader(bytes)
    val bencode = Bencode.string(string).get

    bencode.unfold should be(bytes)
  }

  it must "be able to unfold a positive integer" in {
    val bytes = "i446e".getBytes("US-ASCII")
    val number = new ByteReader(bytes)
    val bencode = Bencode.int(number).get

    bencode.unfold should be(bytes)
  }

  it must "be able to unfold a negative integer" in {
    val bytes = "i-98e".getBytes("US-ASCII")
    val number = new ByteReader(bytes)
    val bencode = Bencode.int(number).get

    bencode.unfold should be(bytes)
  }

  it must "be able to unfold a list" in {
    val bytes = "l4:spam4:eggse".getBytes("US-ASCII")
    val list = new ByteReader(bytes)
    val bencode = Bencode.list(list).get
    
    bencode.unfold should be(bytes)
  }
  
  it must "be able to unfold a map" in {
    val bytes = "d9:publisher3:bob3:agei45e17:publisher-webpage15:www.example.com18:publisher.location4:homee".getBytes("US-ASCII")
    val dict = new ByteReader(bytes)
    val bencode = Bencode.dict(dict).get
    
    bencode.unfold should be(bytes)
    
  }

}