package org.rabbitscript
import util.parsing.combinator._
import trees._
import Function.const

trait RabbitIndentParser {
  self: RabbitParser =>

  type PA = Parser[Any]
  def indent(i: Int): Parser[Unit] = rep(" ") ~ "\n" ~ (" " * i) ^^ const()
  def indent3(beforeIndent: => PA = "")(i: Int)(afterIndent: => PA = ""): Parser[Unit] =
    indent4(beforeIndent)(i)(afterIndent)(afterIndent)
  def indent4(beforeIndent: => PA = "")(i: Int)(whenIndent: => PA = "")(whenNoIndent: => PA = ""): Parser[Unit] =
    (beforeIndent ~> indent(i) ~> whenIndent | whenNoIndent) ^^ const()
}

trait RabbitTokenParser {
  self: RabbitParser =>

  def identifier: Parser[String] =
    """[a-z][a-zA-Z0-9_]*""".r ^? ({
      case id if !(reserved contains id) => id
    }, id => s"""reserved word "$id" can't be used as identifier""")

  def num: Parser[ValueTree] = (
      ("+" | "-").? ~ (
          "[1-9][0-9]*+".r ^^ (_.toInt)
        | "0" ^^ const(0)
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
}

trait RabbitTypeParser {
  self: RabbitParser =>

  def tterm: Parser[RabbitTree] = identifier ^^ StringTree("#")
}

class RabbitParser extends RegexParsers with RabbitIndentParser with RabbitTokenParser with RabbitTypeParser{
  override val skipWhitespace = false
  val reserved = List("var", "function")

  def expr(i: Int): Parser[RabbitTree] = (
      identifier ~ (rep(" ") ~ ":" ~ rep(" ") ~ "=" ~ indent4(rep(" "))(i + 2)()(rep(" "))) ~ expr1(i) ^^ {
        case n ~ _ ~ v => VarDefTree(n, v)
      }
    | expr1(i)
  )

  def expr1(i: Int): Parser[RabbitTree] = (
    expr2(i) ~ (indent4(rep(" "))(i + 2)()(rep(" ")) ~ ("+" | "-") ~ indent4(rep(" "))(i + 2)()(rep1(" ")) ~ expr2(i)).* ^^ {
      case x ~ list => (x /: list) {(l, y) =>
        y match {
          case _ ~ op ~ _ ~ r => TwoOpTree(op, l, r)
        }
      }
    }
  )

  def expr2(i: Int): Parser[RabbitTree] = (
    expr3(i) ~ (
      indent4(rep(" "))(i + 2)()(rep(" ")) ~ ("*" | "//" | "/" | "%%" | "%") ~ indent4(rep(" "))(i + 2)()(rep1(" ")) ~ expr3(i)
    ).* ^^ {
      case x ~ list => (x /: list) {(l, y) =>
        y match {
          case _ ~ op ~ _ ~ r => TwoOpTree(op, l, r)
        }
      }
    }
  )

  def expr3(i: Int): Parser[RabbitTree] = (
      term(i) ~ indent4(rep(" "))(i + 2)()(rep(" ")) ~ "**" ~ indent4(rep(" "))(i + 2)()(rep1(" ")) ~ expr3(i) ^^ {
        case l ~ _ ~ op ~ _ ~ r => TwoOpTree(op, l, r)
      }
    | term(i)
  )

  def term(i: Int): Parser[RabbitTree] =
    num | string | (
      "(" ~> indent4(rep(" "))(0)(rep(" "))(rep(" ")) ~> expr(0) <~ indent4(rep(" "))(i)(rep(" "))(rep(" ")) <~ ")"
    ) | failure("no term found")

  def parse(s: String) = parseAll(expr(0), s)
}
