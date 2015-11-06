package com.joeledstrom

import java.text.ParseException

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

  def stringLiteral: Parser[String] = intLiteral ~> ":" ~> "\"[^\"]*\"".r ^^ (s => s.tail.dropRight(1))

  def intLiteral: Parser[Long] = "[0-9]*".r ^? (
    { case s if Try(s.toLong).isSuccess => s.toLong },
    { _ => "Error parsing integer" }
  )

  def int: Parser[PhpValue] = "i:" ~ intLiteral ~ ";" ^^ { case _ ~ i ~ _ => PhpInt(i)}

  def string: Parser[PhpValue] = "s:" ~ stringLiteral ~ ";" ^? (
    { case (_ ~ len ~ _ ~ str ~ _) if (len == str.getBytes("utf-8").length) => PhpString(str) },
    { _ => "Actual string length doesn't match specified length" }
  )


  def array: Parser[PhpValue] = "a:" ~ arrayPart ^^ { case _ ~ a => a}

  def arrayPart: Parser[PhpValue] = intLiteral ~ ":{" ~ (((string | int) ~ anyValue)*) ~ "}" ^^
    { case (i ~ _ ~ contents ~ _) => PhpArray(contents.map { case (k ~ v) => (k, v) })}

  def obj: Parser[PhpValue] = "O:" ~> stringLiteral ~ ":" ~ arrayPart ^^
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
