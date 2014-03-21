package org.gszeliga.potok.parser

import scala.io.Source
import scala.io.Codec
import java.io.FileInputStream
import scala.collection.immutable.Stream
import scala.util.parsing.input.CharArrayReader

object test {

  val fis = new FileInputStream(getClass().getResource("/samples/The.Walking.Dead.S04E01.HDTV.x264-ASAP.eztv.torrent").getFile())
                                                  //> fis  : java.io.FileInputStream = java.io.FileInputStream@3b362ca8

	Stream.continually(fis.read).map(_.toByte.toChar)
                                                  //> res0: scala.collection.immutable.Stream[Char] = Stream(d, ?)
 
 
 val aa = 0x75.toByte.toChar                      //> aa  : Char = u
 val bb = 0x31.toChar                             //> bb  : Char = 1
 new CharArrayReader("4:test".toCharArray())      //> res1: scala.util.parsing.input.CharArrayReader = scala.util.parsing.input.Ch
                                                  //| arArrayReader@2d33886b
 
 List(("Hola",1)).toMap                           //> res2: scala.collection.immutable.Map[String,Int] = Map(Hola -> 1)
 
}