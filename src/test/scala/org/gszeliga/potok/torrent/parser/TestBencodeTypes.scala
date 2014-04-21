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
    
    bencode.unfold should be (bytes)
  }
  
  it must "be able to unfold a positive integer" in {
    val bytes = "i446e".getBytes("US-ASCII")
    val number = new ByteReader(bytes)
    val bencode = Bencode.int(number).get
    
    bencode.unfold should be (bytes)
  }
  
  it must "be able to unfold a negative integer" in {
    val bytes = "i-98e".getBytes("US-ASCII")
    val number = new ByteReader(bytes)
    val bencode = Bencode.int(number).get
    
    bencode.unfold should be (bytes)
  }
  
}