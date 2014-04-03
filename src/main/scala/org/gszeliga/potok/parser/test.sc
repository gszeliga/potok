package org.gszeliga.potok.parser

import scala.io.Source
import scala.io.Codec
import java.io.FileInputStream
import scala.collection.immutable.Stream
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader

object test {

  List(" ", " ").mkString                         //> res0: String = "  "

  'ç'.toHexString                                 //> res1: String = e7
  "%02X".format('ç'.toByte)                       //> res2: String = E7
  0xe7                                            //> res3: Int(231) = 231

  'ç'.toHexString                                 //> res4: String = e7
  'ñ'.toHexString                                 //> res5: String = f1
  'l'.toByte                                      //> res6: Byte = 108

  254.toByte & 0xff                               //> res7: Int = 254
}