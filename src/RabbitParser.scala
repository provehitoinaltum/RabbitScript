package org.rabbitscript
import util.parsing.combinator._

object RabbitParser extends RegexParsers {
  import trees._
  override val skipWhitespace = false
  def expr: Parser[RabbitTree] = expr1
  def expr1: Parser[RabbitTree] = expr2
  def expr2: Parser[RabbitTree] = expr3
  def expr3: Parser[RabbitTree] = expr4
  def expr4: Parser[RabbitTree] = expr5
  def expr5: Parser[RabbitTree] = expr6
  def expr6: Parser[RabbitTree] = term
  def term: Parser[RabbitTree] = num | string
  def num: Parser[ValueTree] = """-?+[1-9][0-9]*+|0""".r ~ """\.[0-9]++""".r.? ^^ {
    case intPart ~ Some(decPart) => FloatTree((intPart + decPart).toDouble)
    case intPart ~ None => IntTree(intPart.toInt)
  }
  def oneQuoteString(quote: String): Parser[StringTree] = (
    quote ~>
      rep(
          "\\" ~! (
              """[0btnvfr"'\\\n]""".r ^^ {
                  case "\n" => ""
                  case "\\" => "\\"
                  case "\"" => "\""
                  case "'"  => "'"
                  case "t"  => "\t"
                  case "n"  => "\n"
                  case "r"  => "\r"
                  case "0"  => "\0"
              }
            | """u[0-9a-f]{4}""".r ^^ {
                case s => 
                  Integer.parseInt(s.slice(1, 5), 16).toChar.toString
              }
            | failure("illegal escape sequence")
          ) ^^ { case _ ~ v => v }
        | s"[^$quote\\\\]+".r
      )
    <~ (quote | failure("illegal end of string"))
    ^^ (_.mkString) ^^ StringTree(quote)
  )
  def threeQuoteString(quote: String): Parser[StringTree] = (
    repN(3, quote) ~>
      rep(s"$quote{0,2}".r ~ (
            s"[^$quote#]+".r
/*
          | "#{" ~> opt(expr) <~ "}" ^^ {
              case Some(expr)
            }
*/
        ) ^^ { case q ~ s => q + s }
      )
    <~ (repN(3, quote) | failure("illegal end of string"))
    ^^ (_.mkString) ^^ StringTree(quote)
  )
  def string: Parser[StringTree] = threeQuoteString("\"") | threeQuoteString("'") | oneQuoteString("\"") | oneQuoteString("'")
  def parse(s: String) = parseAll(expr, s)
}
