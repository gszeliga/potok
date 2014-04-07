package org.gszeliga.potok.torrent

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.gszeliga.potok.torrent.parser.Bencode

@RunWith(classOf[JUnitRunner])
class TestTorrent extends FlatSpec with Matchers {

  behavior of "Torrent"

  val torrent = Bencode.parse(getClass().getResource("/samples/torrent/The.Walking.Dead.S04E01.HDTV.x264-ASAP.eztv.torrent")) match {
    case Left(t) => Torrent.from(t)
    case Right(e) => fail("Torrent file could not be parsed")
  }

  it must "find an existing key" in {
    torrent / "announce" shouldBe a[Continue[_]]
  }

  it must "fail finding a non-existing key" in {
    torrent / "dummy" shouldBe a[Finish.type]
  }

  it must "retrieve an existing key value as string" in {
    val str = torrent / "announce" asString

    str should be("udp://tracker.openbittorrent.com:80/")
  }

  it must "be able to chain keys" in {
    torrent / "info" / "name" shouldBe a[Continue[_]]
  }

  it must "be able to chain keys and retrieve value as string" in {
    val value = torrent / "info" / "name" asString

    value should be("The.Walking.Dead.S04E01.HDTV.x264-ASAP.mp4")
  }

}