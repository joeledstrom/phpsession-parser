package com.joeledstrom

import java.text.ParseException

import scala.annotation.tailrec
import scala.util.Try
import scala.util.parsing.input.CharSequenceReader

/**
  * Created by joel on 04/11/15.
  */
trait PhpValue

case class PhpObject(name: String, contents: PhpArray) extends PhpValue
case class PhpArray(values: Seq[(PhpValue, PhpValue)]) extends PhpValue
case class PhpString(value: String) extends PhpValue
case class PhpInt(value: Long) extends PhpValue

object PhpParser extends scala.util.parsing.combinator.RegexParsers {



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

      val b = new StringBuilder
      var byteCount = 0
      var reader = in.rest

      while (byteCount < byteLength) {
        val c = reader.first

        if (c == CharSequenceReader.EofCh) return None

        if (c <= 0x7F) {
          byteCount += 1
        } else if (c <= 0x7FF) {
          byteCount += 2
        } else if (c.isHighSurrogate) {
          byteCount += 4
          reader = reader.rest
        } else {
          byteCount += 3
        }

        b.append(c)
        reader = reader.rest
      }

      if (reader.first == '"' && byteCount == byteLength)
        Some(reader.rest, b.toString())
      else
        None
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

  def int: Parser[PhpValue] = "i:" ~ intParser ~ ";" ^^ { case _ ~ i ~ _ => PhpInt(i)}

  def string = "s:" ~> stringParser <~ ";" ^^ (x => PhpString(x))



  def array: Parser[PhpValue] = "a:" ~ arrayPart ^^ { case _ ~ a => a}

  def arrayPart: Parser[PhpValue] = intParser ~ ":{" ~ (((string | int) ~ anyValue)*) ~ "}" ^^
    { case (i ~ _ ~ contents ~ _) => PhpArray(contents.map { case (k ~ v) => (k, v) })}

  def obj: Parser[PhpValue] = "O:" ~> stringParser ~ ":" ~ arrayPart ^^
    { case (name ~ _ ~ PhpArray(v)) => PhpObject(name, PhpArray(v)) }

  val anyValue: Parser[PhpValue] = array | obj | string | int

  def sessionName: Parser[String] = """[^\|]+\|""".r ^^ { case n => n.dropRight(1) }

  def session: Parser[PhpObject] = sessionName ~ anyValue ^^
    { case name ~ PhpArray(c) => PhpObject(name, PhpArray(c))}

  def sessions = (session*)



  def parseSerialized(s: String) = parse(s, anyValue)
  def parseSession(s: String) = parse(s, sessions)
  private def parse[T](s:String, parser: Parser[T]): Try[T] = {
    val phraseParser = phrase(parser)
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
      case Success(t,_)     => scala.util.Success[T](t)
      case NoSuccess(msg, input) => scala.util.Failure(new ParseException(msg, input.offset))
    }
  }
}
