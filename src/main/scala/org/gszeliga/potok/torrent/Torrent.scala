package org.gszeliga.potok.torrent

import org.gszeliga.potok.torrent.parser.BencodeType
import org.gszeliga.potok.torrent.parser.BDict
import org.gszeliga.potok.torrent.parser.BString
import org.gszeliga.potok.torrent.parser.BInt
import java.util.Date
import com.github.nscala_time.time.Imports._
import org.gszeliga.potok.torrent.parser.BList

sealed trait Torrent[+A] {

  def /[B <: BencodeType](key: String): Torrent[B] = get(key)
  def ?[B](key: String): Boolean = exist(key)  
  
  def exist[B](key: String): Boolean = get(key).found
  def get[B <: BencodeType](key: String): Torrent[B]

  def raw: A
  def asString: Option[String]
  def found: Boolean
  def asInt: Option[Int]
  def asDate: Option[Date]
  def asList[B]: Option[List[B]]
  def asFlatList[B]: Option[List[B]] = {

    def flatList[B](l: List[_]): List[B] = l match {
      case Nil => Nil
      case (head: List[_]) :: tail => flatList(head) ::: flatList(tail)
      case head :: tail => head.asInstanceOf[B] :: flatList(tail)
    }

    asList map flatList

  }

  def map[B](f: A => B): Torrent[B]
  def flatten[B](v: Torrent[Torrent[B]]): Torrent[B] = v.raw
  def flatMap[B](f: A => Torrent[B]): Torrent[B] = {
    flatten(map(f))
  }

}

object Finish extends Torrent[Nothing] {
  def raw = throw new IllegalAccessException("There's no value available to be retrieved")
  def get[B](key: String) = this
  def asString = None
  def asInt = None
  def asDate = None
  def asList[B] = None
  def map[B](f: Nothing => B) = this
  def found: Boolean = false
}

class Found[A](val raw: A) extends Torrent[A] {

  def found: Boolean = true
  def map[B](f: A => B): Torrent[B] = new Found(f(raw))

  def get[B <: BencodeType](key: String) = {
    raw match {
      case BDict(m) => {
        m.get(BString(key.getBytes("US-ASCII").toList)) map (t => new Found(t.asInstanceOf[B])) getOrElse (Finish)
      }
      case _ => Finish
    }
  }

  def asString = {
    raw match {
      case BString(l) => Some(new String(l.toArray, "ISO-8859-15"))
      case _ => None
    }
  }

  def asInt = {
    raw match {
      case BInt(i) => Some(i)
      case _ => None
    }
  }

  //Standard UNIX epoch format (integer, seconds since 1-Jan-1970 00:00:00 UTC)
  def asDate = asInt map (s => new Date(s.seconds.millis))

  def asList[B] = {
    raw match {
      case BList(l) => {

        l.map(new Found(_)).foldRight(Option(List.empty[B])) { (c, acc) =>
          for {
            v <- c.asInt.orElse(c.asString).orElse(c.asList);
            l <- acc
          } yield v.asInstanceOf[B] :: l

        }
      }
      case _ => None
    }
  }

}

object Torrent {
  implicit def from[A <: BencodeType](bencode: A): Torrent[A] = {
    new Found(bencode)
  }
}