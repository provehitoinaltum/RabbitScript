package org.rabbitscript
import util.parsing.combinator._
import trees._
import Function.const

trait RabbitIndentParser {
  self: RabbitParser =>

  object indent extends (Int => Parser[Unit]) {
    def apply(i: Int): Parser[Unit] = "\n" ~ (" " * i) ^^ const()
    def *(i: Int): Parser[Unit] = rep(" ") ~> this(i)
    def *|*(i: Int): Parser[Unit] = rep(" ") ~ this(i).? ^^ const()
    def *|+(i: Int): Parser[Unit] = rep(" ") ~> this(i) | rep1(" ") ^^ const()
    def **(i: Int): Parser[Unit] = rep(" ") ~> this(i) <~ rep(" ")
    def **|*(i: Int): Parser[Unit] = rep(" ") ~ (this(i) ~ rep(" ")).? ^^ const()
    def **|+(i: Int): Parser[Unit] = rep(" ") ~> this(i) <~ rep(" ") | rep1(" ") ^^ const()
  }
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
/*
      identifier ~ (rep(" ") ~ ("\n" ~ rep(" ")).? ~ ":" ~ rep(" ") ~ "=" ~ rep(" ") ~ ("\n" ~ rep(" ")).?) ~ expr1(i) ^^ {
*/
      identifier ~ (indent.**|*(0) ~ ":" ~ rep(" ") ~ "=" ~ indent.**|*(0)) ~ expr1(i) ^^ {
        case n ~ _ ~ v => VarDefTree(n, v)
      }
    | expr1(i)
  )

  def expr1(i: Int): Parser[RabbitTree] = (
    expr2(i) ~ rep(
      (indent.**|*(0) ~> ("==" | "!=" | "<" | "<=" | ">" | ">=") <~ indent.**|+(0)) ~ expr2(i)
    ) ^^ {
      case x ~ list => (x /: list) {(l, y) =>
        y match {
          case op ~ r => BinaryOpTree(op, l, r)
        }
      }
    }
  )

  def expr2(i: Int): Parser[RabbitTree] = (
    expr3(i) ~ rep(
      (indent.**|*(0) ~> ("+" | "-") <~ indent.**|+(0)) ~ expr3(i)
    ) ^^ {
      case x ~ list => (x /: list) {(l, y) =>
        y match {
          case op ~ r => BinaryOpTree(op, l, r)
        }
      }
    }
  )

  def expr3(i: Int): Parser[RabbitTree] = (
    expr4(i) ~ rep(
      (indent.**|*(0) ~> ("*" | "//" | "/" | "%%" | "%") <~ indent.**|+(0)) ~ expr4(i)
    ) ^^ {
      case x ~ list => (x /: list) {(l, y) =>
        y match {
          case op ~ r => BinaryOpTree(op, l, r)
        }
      }
    }
  )

  def expr4(i: Int): Parser[RabbitTree] = (
      term(i) ~ (indent.**|*(0) ~> "**" <~ indent.**|+(0)) ~ expr4(i) ^^ {
        case l ~ op ~ r => BinaryOpTree(op, l, r)
      }
    | term(i)
  )

  def term(i: Int): Parser[RabbitTree] = (
      num
    | ("+" | "-" | "*" | "/" | "%") ~ term(i) ^^ {
        case op ~ v => UnaryOpTree(op, v)
      }
    | string
    | "(" ~> indent.**|*(0) ~> expr(0) <~ indent.**|*(0) <~ ")"
    | identifier ^^ VarRefTree
    | failure("no term found")
  )

  def block(i: Int): Parser[BlockTree] =
    repsep(expr(i).?, indent.*(i)) ^^ (xs => BlockTree(xs collect {case Some(x) => x}))

  def parse(s: String) = parseAll(block(0), s)
}
