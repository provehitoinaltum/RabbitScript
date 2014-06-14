package org.rabbitscript
import util.parsing.combinator._

trait RabbitTypeParser {
  self: RabbitParser =>
  import trees._
  def tterm: Parser[RabbitTree] = identifier ^^ StringTree("#")
}

class RabbitParser extends RegexParsers with RabbitTypeParser{
  import trees._
  override val skipWhitespace = false
  val reserved = List("var", "function")

  def identifier: Parser[String] =
    """[a-z][a-zA-Z0-9_]*""".r ^? ({
      case id if !(reserved contains id) => id
    }, id => s"""reserved word "$id" can't be used as identifier""")

  def num: Parser[ValueTree] = (
      ("+" | "-").? ~ (
          "[1-9][0-9]*+".r ^^ (_.toInt)
        | "0" ^^ Function.const(0)
        | guard("""\.[0-9]""".r) ~> err("number literal must have integer part(not only decimal point)")
      ) ~ (
        "." ~> (
            "[0-9]++".r
          | err("decimal number literal must have decimal part (not only decimal point)")
        )
      ).? ^^ {
        case Some("-") ~ intPart ~ Some(decPart) => FloatTree(-(intPart + "." + decPart).toDouble)
        case _         ~ intPart ~ Some(decPart) => FloatTree((intPart + "." + decPart).toDouble)
        case Some("-") ~ intPart ~ None => IntTree(-intPart)
        case _         ~ intPart ~ None => IntTree(intPart)
      }
    | ("+" | "-") ~> failure("number literal expected but not found")
  )

  def oneQuoteString(quote: String): Parser[StringTree] = (
    quote ~>
      rep(
          "\\" ~> (
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
          )
        | s"[^$quote\\\\]+".r
      )
    <~ (quote | failure("illegal end of string"))
    ^^ (_.mkString) ^^ StringTree(quote)
  )

  def threeQuoteString(quote: String): Parser[StringTree] = (
    repN(3, quote) ~>
      rep(s"$quote{0,2}".r ~ (
            s"[^$quote]+".r
//        | ...
        ) ^^ { case q ~ s => q + s }
      )
    <~ (repN(3, quote) | failure("illegal end of string"))
    ^^ (_.mkString) ^^ StringTree(quote)
  )

  def string: Parser[StringTree] = (
    threeQuoteString("\"") | threeQuoteString("'")
    | oneQuoteString("\"") | oneQuoteString("'")
  )

  def expr: Parser[RabbitTree] = (
      identifier ~ (rep(" ") ~ ":" ~ rep(" ") ~ "=" ~ rep(" ")) ~ expr1 ^^ {
        case n ~ _ ~ v => VarDefTree(n, v)
      }
    | expr1
  )

  def expr1: Parser[RabbitTree] = (
    expr2 ~ (rep(" ") ~ ("+" | "-") ~ rep1(" ") ~ expr2).* ^^ {
      case x ~ list => (x /: list) {(l, y) =>
        y match {
          case _ ~ op ~ _ ~ r => TwoOpTree(op, l, r)
        }
      }
    }
  )

  def expr2: Parser[RabbitTree] = (
    expr3 ~ (rep(" ") ~ ("*" | "//" | "/" | "%%" | "%") ~ rep1(" ") ~ expr3).* ^^ {
      case x ~ list => (x /: list) {(l, y) =>
        y match {
          case _ ~ op ~ _ ~ r => TwoOpTree(op, l, r)
        }
      }
    }
  )

  def expr3: Parser[RabbitTree] = (
      term ~ rep(" ") ~ "**" ~ rep1(" ") ~ expr3 ^^ {
        case l ~ _ ~ op ~ _ ~ r => TwoOpTree(op, l, r)
      }
    | term
  )

  def term: Parser[RabbitTree] = num | string | ("(" ~> rep(" ") ~> expr <~ rep(" ") <~ ")") | failure("no term found")

  def parse(s: String) = parseAll(expr, s)
}
