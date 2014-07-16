package org.rabbitscript
import net.akouryy.common.Lib._

import util.parsing.combinator._
import trees._
import Function.const

trait RabbitSpaceParser {
  self: RabbitParser ⇒

  /** indent */
  def  ind    (i: Int) = "\n" ~ (" " * i) ^^ const()
  /** space* indent */
  def sind    (i: Int) = rep(" ") ~ ind(i) ^^ const()
  /** space* indent space* */
  def sinds   (i: Int) = sind(i) ~ rep(" ") ^^ const()
  /** indent? */
  def  indp   (i: Int) = ind(i).? ^^ const()
  /** space* indent? */
  def sindp   (i: Int) = rep(" ") ~ indp(i) ^^ const()
  /** space* indent? space* */
  def sindps  (i: Int) = rep(" ") ~ (ind(i) ~ rep(" ")).? ^^ const()
  /** indent | space+ */
  def  ind_ss (i: Int) = ind(i) | rep1(" ") ^^ const()
  /** space* indent | space+ */
  def sind_ss (i: Int) = sind(i) | rep1(" ") ^^ const()
  /** space* indent space* | space+ */
  def sinds_ss(i: Int) = sinds(i) | rep1(" ") ^^ const()

  private def some[T, U](f: Option[T] ⇒ U) = (x: T) ⇒ f(Some(x))
  def with_ind     [T](i: Int)(f: Int ⇒ Parser[T]) = ind(i) ~> f(i)
  def with_sind    [T](i: Int)(f: Int ⇒ Parser[T]) = sind(i) ~> f(i)
  def with_sinds   [T](i: Int)(f: Int ⇒ Parser[T]) = {
    var x = 0
    sind(i) ~ (rep(" ") ^^ {s ⇒ x = s.length}) ~> f(i + x)
  }
  def with_indp    [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = ind(i) ~> f(Some(i)) | f(None)
  def with_sindp   [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = sind(i) ~> f(Some(i)) | rep(" ") ~> f(None)
  def with_sindps  [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = with_sinds(i)(some(f)) | rep(" ") ~> f(None)
  def with_ind_ss  [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = ind(i) ~> f(Some(i)) | rep1(" ") ~> f(None)
  def with_sind_ss [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = sind(i) ~> f(Some(i)) | rep1(" ") ~> f(None)
  def with_sinds_ss[T](i: Int)(f: Option[Int] ⇒ Parser[T]) = with_sinds(i)(some(f)) | rep1(" ") ~> f(None)

/*
  implicit class IndentParser[T](p: Parser[T]) {
    def ~\(i: Int): Parser[T]     = p <~ ind(i)
    def ~*\(i: Int): Parser[T]    = p <~ sind(i)
    def ~*\*(i: Int): Parser[T]   = p <~ sinds(i)
    def ~\?(i: Int): Parser[T]    = p <~ indp(i)
    def ~*\?(i: Int): Parser[T]   = p <~ sindp(i)
    def ~*\?*(i: Int): Parser[T]  = p <~ sindps(i)
    def ~\|+(i: Int): Parser[T]   = p <~ ind_ss(i)
    def ~*\|+(i: Int): Parser[T]  = p <~ sind_ss(i)
    def ~*\*|+(i: Int): Parser[T] = p <~ sinds_ss(i)
  }
  import language.implicitConversions
  implicit def StringToIndentParser(s: String) = IndentParser(s)
  implicit def RegexToIndentParser(r: Regex) = IndentParser(r)
*/

  abstract sealed class Indent
  case class StrictIndent(length: Int) extends Indent
  case object InBrace extends Indent
  case object OneLine extends Indent

  val whitespace: Parser[Unit] = (" " | "\n") ^^ const()
}

trait RabbitTokenParser {
  self: RabbitParser ⇒

  def identifier: Parser[String] =
    """[a-z][a-zA-Z0-9_]*""".r ^? ({
      case id if !(reserved contains id) ⇒ id
    }, id ⇒ s"""reserved word "$id" can't be used as identifier""")

  def num: Parser[RabbitLeaf] = (
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
        case Some("-") ~ intPart ~ Some(decPart) ⇒ FloatNode(-(intPart + "." + decPart).toDouble)
        case _         ~ intPart ~ Some(decPart) ⇒ FloatNode((intPart + "." + decPart).toDouble)
        case Some("-") ~ intPart ~ None ⇒ IntNode(-intPart)
        case _         ~ intPart ~ None ⇒ IntNode(intPart)
      }
    | ("+" | "-") ~> failure("number literal expected but not found")
  )

  def oneQuoteString(quote: String): Parser[StringNode] = (
    quote ~>
      rep(
          "\\" ~> (
              """[0btnvfr"'\\\n]""".r ^^ {
                  case "\n" ⇒ ""
                  case "\\" ⇒ "\\"
                  case "\"" ⇒ "\""
                  case "'"  ⇒ "'"
                  case "t"  ⇒ "\t"
                  case "n"  ⇒ "\n"
                  case "r"  ⇒ "\r"
                  case "0"  ⇒ "\0"
              }
            | """u[0-9a-f]{4}""".r ^^ {
                case s ⇒ 
                  Integer.parseInt(s.slice(1, 5), 16).toChar.toString
              }
            | failure("illegal escape sequence")
          )
        | s"[^$quote\\\\]+".r
      )
    <~ (quote | failure("illegal end of string"))
    ^^ (_.mkString) ^^ StringNode(quote)
  )

  def threeQuoteString(quote: String): Parser[StringNode] = (
    repN(3, quote) ~>
      rep(s"$quote{0,2}".r ~ (
            s"[^$quote]+".r
        ) ^^ { case q ~ s ⇒ q + s }
      )
    <~ (repN(3, quote) | failure("illegal end of string"))
    ^^ (_.mkString) ^^ StringNode(quote)
  )

  def string: Parser[StringNode] = (
    threeQuoteString("\"") | threeQuoteString("'")
    | oneQuoteString("\"") | oneQuoteString("'")
  )

  def bool: Parser[BooleanNode] = (
     "false" ^^ const(BooleanNode(false))
    | "true" ^^ const(BooleanNode(true))
  )
}

trait RabbitPatternParser {
  self: RabbitParser ⇒

  def pattern: Parser[PatternTree] = pattern1

  def pattern1: Parser[PatternTree] = ptype | pattern2

  def ptype: Parser[TypePatternTree] = pattern2 ~ (rep(" ") ~ ":" ~ rep(" ")) ~ tidentifier ^^ {
    case p ~ _ ~ t ⇒ TypePatternTree(t, p)
  }

  def pattern2: Parser[PatternTree] = pvar | ptuple | pterm

  def pterm: Parser[PatternTree] = pvar | ptuple | "(" ~> rep(" ") ~> pattern <~ rep(" ") <~ ")"

  def pvar: Parser[VarPatternNode] = identifier ^^ VarPatternNode

  def ptuple: Parser[TuplePatternTree] =
    ("(" ~ rep(" ")) ~> pattern ~ rep(rep(" ") ~ "," ~ rep(" ") ~> pattern) <~ (rep(" ") ~ ")") ^^ {
      case p0 ~ ps ⇒ TuplePatternTree(p0 :: ps)
    }
}

trait RabbitTypeParser {
  self: RabbitParser ⇒

//  def tterm: Parser[RabbitTree] = identifier ^^ StringNode("#")
  def tidentifier: Parser[String] =
    """[A-Z][a-zA-Z0-9_]*""".r ^? ({
      case id if !(reserved contains id) ⇒ id
    }, id ⇒ s"""reserved word "$id" can't be used as identifier""")
}

class RabbitParser extends RegexParsers with RabbitSpaceParser with RabbitTokenParser with RabbitPatternParser with RabbitTypeParser{
  override val skipWhitespace = false
  val reserved = List("true", "false", "var", "if", "else", "then", "while", "until", "do", "for", "in", "function")

  def stmt(i: Indent): Parser[RabbitTree] = (
      varDef(i)
    | condStmt(i)
    | loopStmt(i)
    | expr(i)
  )

  def varref(i: Indent): Parser[VarRefNode] = identifier ^^ VarRefNode

  def expr(i: Indent): Parser[RabbitTree] = assign(i) | expr1(i)
  def assign = exprBinR(List("="), varref, expr1)({ (_, n, v) ⇒ AssignTree(n, v) }) _

  def exprBinL[T, U](ops: List[Parser[String]], l: Indent ⇒ Parser[T], r: Indent ⇒ Parser[U])
                    (f: (String, T, U) ⇒ T)(i: Indent): Parser[T] = (
    i match {
      case StrictIndent(i) ⇒
        l(StrictIndent(i)) ~ rep(
          with_sindps(i + 1){ oj ⇒
            val j = oj getOrElse i
            (ops reduceLeft (_ | _)) ~ with_sinds_ss(j + 1){ od ⇒
              val d = od getOrElse j
              r(StrictIndent(d))
            }
          }
        )
      case InBrace ⇒
        l(InBrace) ~ rep(
          (whitespace.* ~> (ops reduceLeft (_ | _)) <~ whitespace.+) ~ r(InBrace)
        )
      case OneLine ⇒
        l(OneLine) ~ rep(
          (rep(" ") ~> (ops reduceLeft (_ | _)) <~ rep1(" ")) ~ r(OneLine)
        )
    }
  ) ^^ {
    case x ~ list ⇒ (x /: list) {(l, y) ⇒
      y match {
        case op ~ r ⇒ f(op, l, r)
      }
    }
  }

  def exprBinR[T, U](ops: List[Parser[String]], l: Indent ⇒ Parser[T], r: Indent ⇒ Parser[U])
                    (f: (String, T, U) ⇒ U)(i: Indent): Parser[U] = (
    i match {
      case StrictIndent(i) ⇒
        l(StrictIndent(i)) ~ with_sindps(i + 1){ oj ⇒
          val j = oj getOrElse i
          (ops reduceLeft (_ | _)) ~ with_sinds_ss(j + 1){ od ⇒
            val d = od getOrElse j
            exprBinR(ops, l, r)(f)(StrictIndent(d))
          }
        }
      case InBrace ⇒
        l(InBrace) ~ ((whitespace.* ~> (ops reduceLeft (_ | _)) <~ whitespace.*) ~ exprBinR(ops, l, r)(f)(InBrace))
      case OneLine ⇒
        l(OneLine) ~ ((rep(" ") ~> (ops reduceLeft (_ | _)) <~ rep(" ")) ~ exprBinR(ops, l, r)(f)(OneLine))
    }
  ) ^^ {
    case l ~ (op ~ r) ⇒ f(op, l, r)
  } | r(i)

  def expr1 = exprBinL(List("==", "!=", "<", "<=", ">", ">="), expr2, expr2)(BinaryOpTree) _

  def expr2 = exprBinL(List("+", "-"), expr3, expr3)(BinaryOpTree) _

  def expr3 = exprBinL(List("*", "//", "/", "%%", "%"), expr4, expr4)(BinaryOpTree) _

  def expr4 = exprBinR(List("**"), term, term)(BinaryOpTree) _

  def term(i: Indent): Parser[RabbitTree] = (
      num
    | ("+" | "-" | "*" | "/" | "%") ~ term(i) ^^ {
        case op ~ v ⇒ UnaryOpTree(op, v)
      }
    | string
    | bool
    | "(" ~ whitespace.* ~> stmt(InBrace) <~ whitespace.* ~ ")"
    | varref(i)
    | failure("no term found")
  )

  def varDef(i: Indent): Parser[VarDefTree] = (
    i match {
      case StrictIndent(i) ⇒
        pattern ~ with_sindps(i + 1){ oj ⇒
          val j = oj getOrElse i
          ":=" ~ sindps(j + 1)
        } ~ expr(StrictIndent(i))
      case InBrace ⇒
        pattern ~ (whitespace.* ~ ":=" ~ whitespace.*) ~ expr(InBrace)
      case OneLine ⇒
        pattern ~ (rep(" ") ~ ":=" ~ rep(" ")) ~ expr(OneLine)
    }
  ) ^^ {
    case p ~ _ ~ v ⇒ VarDefTree(p, v)
  }

  def condStmt(i: Indent): Parser[CondTree] = (
    i match {
      case StrictIndent(i) ⇒
        ("if" | "unless") ~ (rep1(" ") ~> expr(OneLine)) ~ with_sindps(i + 1){
          case Some(j) ⇒ block(StrictIndent(j))
          case None ⇒ "then" ~> with_sinds_ss(i + 1){
            case Some(j) ⇒ block(StrictIndent(j))
            case None ⇒ stmt(OneLine)
          }
        } ~ with_sind(i){ _ ⇒
          "else" ~> with_sinds_ss(i + 1){
            case Some(j) ⇒ block(StrictIndent(j))
            case None ⇒ stmt(OneLine)
          }
        }.?
      case InBrace ⇒
        ("if" | "unless") ~ (rep1(" ") ~> expr(OneLine)) ~ with_sindps(1){
          case Some(i) ⇒ block(StrictIndent(i))
          case None ⇒ "then" ~> with_sinds_ss(1){
            case Some(i) ⇒ block(StrictIndent(i))
            case None ⇒ stmt(OneLine)
          }
        } ~ with_sinds(0){ _ ⇒
          "else" ~> with_sinds_ss(1){
            case Some(i) ⇒ block(StrictIndent(i))
            case None ⇒ stmt(OneLine)
          }
        }.?
      case OneLine ⇒ (
        ("if" | "unless") ~ (rep1(" ") ~> expr(OneLine))
        ~ (rep1(" ") ~ "then" ~ rep1(" ") ~> stmt(OneLine))
        ~ (rep1(" ") ~ "else" ~ rep1(" ") ~> stmt(OneLine)).?
      )
    }
  ) ^^ {
    case "if"     ~ i ~ t ~ e ⇒ IfTree(i, t, e)
    case "unless" ~ u ~ t ~ e ⇒ UnlessTree(u, t, e)
  }

  def loopStmt(i: Indent): Parser[LoopTree] = whileStmt(i) | forStmt(i)

  def whileStmt(i: Indent): Parser[LoopTree] = (
    i match {
      case StrictIndent(i) ⇒
        ("while" | "until") ~ (rep1(" ") ~> expr(OneLine)) ~ with_sindps(i + 1){
          case Some(j) ⇒ block(StrictIndent(j))
          case None ⇒ "do" ~> with_sinds_ss(i + 1){
            case Some(j) ⇒ block(StrictIndent(j))
            case None ⇒ stmt(OneLine)
          }
        } ~ with_sind(i){ _ ⇒
          "else" ~> with_sinds_ss(i + 1){
            case Some(j) ⇒ block(StrictIndent(j))
            case None ⇒ stmt(OneLine)
          }
        }.?
      case InBrace ⇒
        ("while" | "until") ~ (rep1(" ") ~> expr(OneLine)) ~ with_sindps(1){
          case Some(i) ⇒ block(StrictIndent(i))
          case None ⇒ "do" ~> with_sinds_ss(1){
            case Some(i) ⇒ block(StrictIndent(i))
            case None ⇒ stmt(OneLine)
          }
        } ~ with_sinds(0){ _ ⇒
          "else" ~> with_sinds_ss(1){
            case Some(i) ⇒ block(StrictIndent(i))
            case None ⇒ stmt(OneLine)
          }
        }.?
      case OneLine ⇒ (
        ("while" | "until") ~ (rep1(" ") ~> expr(OneLine))
        ~ (rep1(" ") ~ "do" ~ rep1(" ") ~> stmt(OneLine))
        ~ (rep1(" ") ~ "else" ~ rep1(" ") ~> stmt(OneLine)).?
      )
    }
  ) ^^ {
    case "while" ~ w ~ d ~ e ⇒ WhileTree(w, d, e)
    case "until" ~ u ~ d ~ e ⇒ UntilTree(u, d, e)
  }

  def forStmt(i: Indent): Parser[LoopTree] = (
    i match {
      case StrictIndent(i) ⇒
        "for" ~> rep1(" ") ~> pattern ~ (rep1(" ") ~ "in" ~ rep(" ") ~> expr(OneLine)) ~ with_sindps(i + 1){
          case Some(j) ⇒ block(StrictIndent(j))
          case None ⇒ "do" ~> with_sinds_ss(i + 1){
            case Some(j) ⇒ block(StrictIndent(j))
            case None ⇒ stmt(OneLine)
          }
        } ~ with_sind(i){ _ ⇒
          "else" ~> with_sinds_ss(i + 1){
            case Some(j) ⇒ block(StrictIndent(j))
            case None ⇒ stmt(OneLine)
          }
        }.?
      case InBrace ⇒
        "for" ~> rep1(" ") ~> pattern ~ (rep1(" ") ~ "in" ~ rep(" ") ~> expr(OneLine)) ~ with_sindps(1){
          case Some(i) ⇒ block(StrictIndent(i))
          case None ⇒ "do" ~> with_sinds_ss(1){
            case Some(i) ⇒ block(StrictIndent(i))
            case None ⇒ stmt(OneLine)
          }
        } ~ with_sinds(0){ _ ⇒
          "else" ~> with_sinds_ss(1){
            case Some(i) ⇒ block(StrictIndent(i))
            case None ⇒ stmt(OneLine)
          }
        }.?
      case OneLine ⇒ (
        "for" ~> (rep1(" ") ~> pattern)
        ~ (rep1(" ") ~ "in" ~ rep1(" ") ~> stmt(OneLine))
        ~ (rep1(" ") ~ "do" ~ rep1(" ") ~> stmt(OneLine))
        ~ (rep1(" ") ~ "else" ~ rep1(" ") ~> stmt(OneLine)).?
      )
    }
  ) ^^ {
    case f ~ i ~ d ~ _ ⇒ ForTree(f, i, d)
  }

  def block(i: Indent): Parser[BlockTree] = (
    i match {
      case StrictIndent(i) ⇒ stmt(StrictIndent(i)).? ~ rep(with_sind(i)(i ⇒ stmt(StrictIndent(i)).?))
      case InBrace ⇒ stmt(InBrace).? ~ rep(sindps(0) ~ ";" ~ sindps(0) ~> stmt(InBrace).?)
      case OneLine ⇒ stmt(InBrace).? ~ rep(rep(" ") ~ ";" ~ rep(" ") ~> stmt(InBrace).?)
    }
  ) ^^ {case x ~ xs ⇒ BlockTree((x :: xs) collect {case Some(x) ⇒ x})}

  def parse(s: String) = parseAll(block(StrictIndent(0)), s)
}
