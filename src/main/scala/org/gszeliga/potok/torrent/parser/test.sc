package org.gszeliga.potok.parser

import scala.io.Source
import scala.io.Codec
import java.io.FileInputStream
import scala.collection.immutable.Stream
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader
import java.util.Date
import com.github.nscala_time.time.Imports._

object test {

  List(" ", " ").mkString                         //> res0: String = "  "

  'ç'.toHexString                                 //> res1: String = e7
  "%02X".format('ç'.toByte)                       //> res2: String = E7
  0xe7                                            //> res3: Int(231) = 231

  'ç'.toHexString                                 //> res4: String = e7
  'ñ'.toHexString                                 //> res5: String = f1
  'l'.toByte                                      //> res6: Byte = 108

  254.toByte & 0xff                               //> res7: Int = 254

  new Date(1381716156.seconds.millis)             //> res8: java.util.Date = Mon Oct 14 10:02:36 CST 2013

  val l = List(List("A", List("C")), List("B"))   //> l  : List[List[Object]] = List(List(A, List(C)), List(B))

  l.flatten                                       //> res9: List[Object] = List(A, List(C), B)

  def flatList(l: List[_]): List[Any] = l match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatList(head) ::: flatList(tail)
    case head :: tail => head :: flatList(tail)
  }                                               //> flatList: (l: List[_])List[Any]

  flatList(l)                                     //> res10: List[Any] = List(A, C, B)
  
  
  "hola".getBytes                                 //> res11: Array[Byte] = Array(104, 111, 108, 97)
  
}