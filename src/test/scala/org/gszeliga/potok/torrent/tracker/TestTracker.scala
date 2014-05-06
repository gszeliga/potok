package org.gszeliga.potok.torrent.tracker

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.net.InetSocketAddress

//http://linuxers.org/howto/how-find-out-active-connections-or-which-ports-are-openlistening-linux
@RunWith(classOf[JUnitRunner])
class TestTracker extends FlatSpec with Matchers {

  behavior of "Tracker"

  //nc -ul 6882
  it must "announce to a tracker" in {
    //TODO Code does not look good, maybe it would be a good idea to turn it into a DSL
    val token = Tracker(6881)(
      (sender, msg) => println(s"Received from $sender: $msg")) { ref =>
        ref.announce(new InetSocketAddress("localhost", 6882), "This is a tracker test!".getBytes)
      }
    
    token.stop
  }

}