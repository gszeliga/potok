package org.gszeliga.potok.torrent

import org.gszeliga.potok.torrent.parser.BencodeType
import org.gszeliga.potok.torrent.parser.BDict
import org.gszeliga.potok.torrent.parser.BString
import org.gszeliga.potok.torrent.parser.BEmpty

trait Torrent[+A]{
  def / [B >: BencodeType](key: String): Torrent[B]
  def asString : String
}

object Finish extends Torrent[Nothing] {
  def / [B >: BencodeType](key: String): Torrent[B] = ???
  def asString : String  = ???
}

class Continue[A](bencode: A) extends Torrent[A] {

  def / [B >: BencodeType](key: String): Torrent[B] = {
    bencode match {
      case BDict(m) => {
        m.get(BString(key.getBytes("US-ASCII").toList)) match {
          case Some(b) => new Continue(b)
          case _ => Finish
        }
      }
    }
  }
  
  def asString : String = {
    bencode match {
      case BString(l) => new String(l.toArray, "ISO-8859-15")
      case _ => ""
    }
  }
  
}

object Torrent {

  def from[A](bencode: A): Torrent[A] = {
    new Continue(bencode)
  }

}