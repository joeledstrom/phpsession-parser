package com.joeledstrom

import java.text.ParseException

import scala.annotation.tailrec
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

/**
  * Created by joel on 04/11/15.
  */

object PhpParser extends RegexParsers {

  def stringParser: Parser[String] = new Parser[String] {

    @tailrec
    def readLength(reader: Input, buffer: String): (Input, Option[Long]) = {
      if (reader.first == ':') Try(buffer.toLong) match {
        case scala.util.Success(l) => (reader.rest, Some(l))
        case _ => (reader.rest, None)
      } else if (reader.first == CharSequenceReader.EofCh) {
        (reader.rest, None)
      } else {
        readLength(reader.rest, buffer + reader.first)
      }
    }

    def readString(in: Input)(byteLength: Long): Option[(Input, String)] = {

      if (in.first != '"') return None

      var b = Seq.empty[Char]
      var byteCount = 0
      var reader = in.rest

      while (byteCount < byteLength) {
        var c = reader.first

        if (c == CharSequenceReader.EofCh) return None

        if (c <= 0x7F) {
          byteCount += 1
        } else if (c <= 0x7FF) {
          byteCount += 2
        } else if (c.isHighSurrogate) {
          byteCount += 4
          b = b :+ c
          reader = reader.rest
          c = reader.first
        } else {
          byteCount += 3
        }

        b = b :+ c
        reader = reader.rest
      }

      if (reader.first == '"' && byteCount == byteLength) {

        Some(reader.rest, new String(b.toArray))
      } else {
        None
      }
    }

    override def apply(in: PhpParser.Input): PhpParser.ParseResult[String] = {

      val (rest, byteLength) = readLength(in, "")

      byteLength.flatMap(readString(rest)) match {
        case Some((rest, str)) => Success(str, rest)
        case _ => Failure("Error parsing string", in)
      }
    }
  }

  def intParser: Parser[Long] = "[0-9]*".r ^? (
    { case s if Try(s.toLong).isSuccess => s.toLong },
    { _ => "Error parsing integer" }
  )

  def intValue = "i:" ~> intParser <~ ";"

  def stringValue = "s:" ~> stringParser <~ ";"

  def boolValue = "b:" ~> intParser <~ ";" ^? (
    { case b if b == 0 || b == 1 => if (b == 0) false else true },
    { _ => "Error parsing boolean" }
  )

  def arrayParser: Parser[Map[Any, Any]] = intParser ~ ":{" ~ rep((stringValue | intValue) ~ anyValue) ~ "}" ^^
    { case (i ~ _ ~ contents ~ _) if i == contents.size => {
      (contents map { case (k ~ v) => (k, v) }).toMap
    }}

  def arrayValue = "a:" ~> arrayParser

  def objValue = "O:" ~> stringParser ~ ":" ~ arrayParser ^^
    { case (name ~ _ ~ array) => (name, array) }

  def anyValue: Parser[Any] = arrayValue | objValue | stringValue | intValue | boolValue

  def sessionName: Parser[String] = """[^\|]+\|""".r ^^ { case n => n.dropRight(1) }

  def session = sessionName ~ arrayValue ^^
    { case name ~ array => (name, array)}

  def sessions = rep(session) ^^ { _.toMap }



  def parseSerialized(s: String) = parse(s, anyValue)
  def parseSession(s: String) = parse(s, sessions)
  private def parse[T](s:String, parser: Parser[T]): Try[T] = {
    val phraseParser = phrase(parser)
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
      case Success(t, _)          => scala.util.Success[T](t)
      case NoSuccess(msg, input)  => scala.util.Failure(new ParseException(msg, input.offset))
    }
  }
}
