package org.gszeliga.potok.parser

import scala.io.Source
import scala.io.Codec
import java.io.FileInputStream
import scala.collection.immutable.Stream
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader

object test {

  val fis = StreamReader(new InputStreamReader(new FileInputStream(getClass().getResource("/samples/torrent/The.Walking.Dead.S04E01.HDTV.x264-ASAP.eztv.torrent").getFile())))
                                                  //> fis  : scala.util.parsing.input.StreamReader = scala.util.parsing.input.Stre
                                                  //| amReader@5289cf1e

  fis.first                                       //> res0: Char = d
  fis.drop(1).first                               //> res1: Char = 8

  (0 to 342).foldLeft((fis, "")) { (a, b) =>

    val str = a._2 + a._1.first
    (a._1.drop(1), str)

  }                                               //> res2: (scala.util.parsing.input.StreamReader, String) = (scala.util.parsing.
                                                  //| input.StreamReader@3f77b3cd,d8:announce36:udp://tracker.openbittorrent.com:8
                                                  //| 0/13:announce-listll35:udp://tracker.openbittorrent.com:80el29:udp://tracker
                                                  //| .publicbt.com:80el26:udp://tracker.istole.it:80el25:udp://open.demonii.com:8
                                                  //| 0el32:udp://tracker.coppersurfer.tk:80el23:udp://tracker.ccc.de:80ee7:commen
                                                  //| t56:Visit #EZTV on EFNet (irc.efnet.info) or http://eztv.it/13:crea)

 List(" ", " ").mkString                          //> res3: String = "  "

}