package org.gszeliga.potok.torrent

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.gszeliga.potok.torrent.parser.Bencode
import org.gszeliga.potok.torrent.Torrent._
import java.util.Date

@RunWith(classOf[JUnitRunner])
class TestTorrent extends FlatSpec with Matchers {

  behavior of "Torrent"

  val torrent = Bencode.parse(getClass().getResource("/samples/torrent/The.Walking.Dead.S04E01.HDTV.x264-ASAP.eztv.torrent")) match {
    case Left(t) => t
    case Right(e) => fail("Torrent file could not be parsed")
  }

  it must "find an existing key" in {
    torrent ? "announce" should be(true)
  }

  it must "fail finding a non-existing key" in {
    torrent ? "dummy" should be(false)
  }

  it must "be able to chain keys" in {
    torrent / "info" / "name" shouldBe a[Found[_]]
  }

  it must "be able to chain keys and retrieve value as string" in {
    val value = torrent / "info" / "name" asString

    value should be(Some("The.Walking.Dead.S04E01.HDTV.x264-ASAP.mp4"))
  }

  it must "retrieve an existing key value as string" in {
    val str = torrent / "announce" asString

    str should be(Some("udp://tracker.openbittorrent.com:80/"))
  }  
  
  it must "retrieve an existing key value as integer" in {
    val integer = torrent / "info" / "length" asInt

    integer should be(Some(568868608))
  }
  
  it must "retrieve an existing key value as date" in {
    
    import com.github.nscala_time.time.Imports._
    
    val date = torrent / "creation date" asDate
    
    date should be(Some(new Date(1381716156.seconds.millis)))
    
  }
  
  it must "retrieve an existing key value as a flattened list" in {
    
    val list = torrent / "announce-list" asFlatList
    
    list should be(Some(List("udp://tracker.openbittorrent.com:80", "udp://tracker.publicbt.com:80", "udp://tracker.istole.it:80", "udp://open.demonii.com:80", "udp://tracker.coppersurfer.tk:80", "udp://tracker.ccc.de:80")))
    
  }
  
}