package org.rabbitscript
import net.akouryy.common._
import Lib._

import util.parsing.combinator._
import trees._

trait ExtParser {
  self: RabbitParser ⇒

//  type ParserExt[+A] = Parser[A] 

  private[this] var indent = 0
  protected[this] var logParser = false
  protected[this] var showColorful = false

  class ParserExt[+A](p: Parser[A], name: String, ind: Option[Indent]) extends Parser[A] {
    override def apply(in: Input) = {
      if(logParser && (List("fnCall", "tupleInner", "tupleInner-1", "varRef") contains name)) {
        ind match {
          case Some(i) ⇒
            if(showColorful) {
              println(
                Console.WHITE + "| " * (indent - 1) + (if(indent==0) "" else "|-") + Console.BOLD + Console.CYAN + name + ResetConsole + "(" + Console.BOLD + i + ResetConsole + ") - " + in
              )
              indent += 1
                val r = p(in)
              indent -= 1
              println("| " * indent + Console.BOLD + "`->" + ResetConsole + " " + (
                r match {
                  case Success(r, n) ⇒
                    Console.BOLD + Console.GREEN  + "Success" + ResetConsole + s"(${n.pos}): " + Console.BOLD + Console.GREEN  + r
                  case Failure(m, n) ⇒
                    Console.BOLD + Console.YELLOW + "Failure" + ResetConsole + s"(${n.pos}): " + Console.BOLD + Console.YELLOW + m.replace("\n", "\\n")
                  case Error(m, n) ⇒
                    Console.BOLD + Console.RED    + "Error"   + ResetConsole + s"(${n.pos}): " + Console.BOLD + Console.RED    + m.replace("\n", "\\n")
                }
              ) + ResetConsole + " [" + Console.CYAN + name + ResetConsole + "]")
              r
            } else {
              println(
                "  " * indent + name + "(" + i + ") - " + in
              )
              indent += 1
                val r = p(in)
              indent -= 1
              println("  " * indent + " -> " + (
                r match {
                  case Success(r, n) ⇒
                    s"Success(${n.pos}): ${r}"
                  case Failure(m, n) ⇒
                    s"Failure(${n.pos}): ${m.replace("\n", "\\n")}"
                  case Error(m, n) ⇒
                    s"Error(${n.pos}): ${m.replace("\n", "\\n")}"
                }
              ) + " [" + name + "]")
              r
            }
          case None ⇒ p(in)
        }
      } else p(in)
    }
  }

  implicit class NamingString(self: (String, Indent)) {
    private val (s, i) = self
    def nameParserExt[A](p: Parser[A]) = new ParserExt(p named s, s, Some(i))
  }

  def noNameParserExt[A](p: Parser[A]) = new ParserExt(p, "", None)
}


trait RabbitSpaceParser {
  self: RabbitParser ⇒

  /** indent */
  def  ind    (i: Int) = "\n" ~ (" " * i) ^^^ (())
  /** space* indent */
  def sind    (i: Int) = rep(" ") ~ ind(i) ^^^ (())
  /** space* indent space* */
  def sinds   (i: Int) = sind(i) ~ rep(" ") ^^^ (())
  /** indent? */
  def  indp   (i: Int) = ind(i).? ^^^ (())
  /** space* indent? */
  def sindp   (i: Int) = rep(" ") ~ indp(i) ^^^ (())
  /** space* indent? space* */
  def sindps  (i: Int) = rep(" ") ~ (ind(i) ~ rep(" ")).? ^^^ (())
  /** indent | space+ */
  def  ind_ss (i: Int) = ind(i) | rep1(" ") ^^^ (())
  /** space* indent | space+ */
  def sind_ss (i: Int) = sind(i) | rep1(" ") ^^^ (())
  /** space* indent space* | space+ */
  def sinds_ss(i: Int) = sinds(i) | rep1(" ") ^^^ (())

  private def some[T, U](f: Option[T] ⇒ U) = (x: T) ⇒ f(Some(x))
  def with_ind     [T](i: Int)(f: Int ⇒ Parser[T]) = ind(i) ~> f(i)
  def with_sind    [T](i: Int)(f: Int ⇒ Parser[T]) = sind(i) ~> f(i)
  def with_sinds   [T](i: Int)(f: Int ⇒ Parser[T]) = sind(i) ~> rep(" ") >> { case s ⇒ f(i + s.length) }
/*
  def with_sinds   [T](i: Int)(f: Int ⇒ Parser[T]) = {
    var x = 0
    sind(i) ~ (rep(" ") ^^ {s ⇒ x = s.length}) ~> f(i + x)
  }
*/
  def with_indp    [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = ind(i) ~> f(Some(i)) | f(None)
  def with_sindp   [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = sind(i) ~> f(Some(i)) | rep(" ") ~> f(None)
  def with_sindps  [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = with_sinds(i)(f compose Some.apply) | rep(" ") ~> f(None)
  def with_ind_ss  [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = ind(i) ~> f(Some(i)) | rep1(" ") ~> f(None)
  def with_sind_ss [T](i: Int)(f: Option[Int] ⇒ Parser[T]) = sind(i) ~> f(Some(i)) | rep1(" ") ~> f(None)
  def with_sinds_ss[T](i: Int)(f: Option[Int] ⇒ Parser[T]) = with_sinds(i)(f compose Some.apply) | rep1(" ") ~> f(None)

  abstract sealed class Indent
  case class StrictIndent(length: Int) extends Indent
  case object InBrace extends Indent
  case object OneLine extends Indent

  val whitespace: Parser[Unit] = (" " | "\n") ^^^ (())
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
        | "0" ^^^ 0
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
                  case "0"  ⇒ "\u0000"
              }
            | """u[0-9a-f]{4}""".r ^^ {
                case s ⇒ 
                  Integer.parseInt(s.slice(1, 5), 16).toChar.toString
              }
            | failure("illegal escape sequence")
          )
        | s"""[^$quote\\]+""".r
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
     "false" ^^^ BooleanNode(false)
    | "true" ^^^ BooleanNode(true)
  )
}

trait RabbitPatternParser {
  self: RabbitParser ⇒

  def pattern: Parser[PatternTree] = pattern1

  def pattern1: Parser[PatternTree] = ptype | pattern2

  def ptype: Parser[TypePatternTree] = pattern2 ~ (rep(" ") ~ "[" ~ rep(" ")) ~ tidentifier ~ (rep(" ") ~ "}" ~ rep(" ")) ^^ {
    case p ~ _ ~ t ~ _ ⇒ TypePatternTree(t, p)
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

class RabbitParser(log: Boolean, color: Boolean) extends RegexParsers
    with RabbitSpaceParser with RabbitTokenParser with RabbitPatternParser with RabbitTypeParser with ExtParser{
  logParser = log
  showColorful = color

  override val skipWhitespace = false
  val reserved = List("true", "false", "var", "if", "else", "then", "while", "until", "do", "for", "in", "function")

  def stmt(i: Indent): ParserExt[RabbitTree] = ("stmt", i) nameParserExt(
      varDef(i)
    | condStmt(i)
    | loopStmt(i)
    | assign(i)
  )

  def varRef(i: Indent): ParserExt[VarRefNode] = ("varRef", i) nameParserExt identifier ^^ VarRefNode

  def assign = binOpR[VarRefNode, RabbitTree]("assign", List("="), varRef, fnCall, { (_, n, v) ⇒ AssignTree(n, v) }) _

  def binOpL[A >: B, B](name: String, ops: List[Parser[String]], l: Indent ⇒ Parser[A], r: Indent ⇒ Parser[B], f: (String, A, B) ⇒ A)(i: Indent): ParserExt[A] =
    noNameParserExt(
      (name, i) nameParserExt (
        i match {
          case StrictIndent(i) ⇒
            l(StrictIndent(i)) ~ rep1(
              with_sindps(i + 1){ oj ⇒
                val j = oj getOrElse i
                (ops reduceLeft (_ | _)) ~ with_sinds_ss(j + 1){ od ⇒
                  val d = od getOrElse j
                  r(StrictIndent(d))
                }
              }
            )
          case InBrace ⇒
            l(InBrace) ~ rep1(
              (whitespace.+ ~> (ops reduceLeft (_ | _)) <~ whitespace.+) ~ r(InBrace)
                |             ((ops reduceLeft (_ | _)) <~ whitespace.*) ~ r(InBrace)
            )
          case OneLine ⇒
            l(OneLine) ~ rep1(
              (rep1(" ") ~> (ops reduceLeft (_ | _)) <~ rep1(" ")) ~ r(OneLine)
                |          ((ops reduceLeft (_ | _)) <~ rep (" ")) ~ r(OneLine)
            )
        }
      ) ^^ {
        case x ~ list ⇒ (x /: list) {(l, y) ⇒
          y match {
            case op ~ r ⇒ f(op, l, r)
          }
        }
      } | l(i)
    )

  def binOpR[A, B >: A](name: String, ops: List[Parser[String]], l: Indent ⇒ Parser[A], r: Indent ⇒ Parser[B], f: (String, A, B) ⇒ B)(i: Indent): ParserExt[B] =
    noNameParserExt(
      (
        (name, i) nameParserExt (
          i match {
            case StrictIndent(i) ⇒
              l(StrictIndent(i)) ~ with_sindps(i + 1){ oj ⇒
                val j = oj getOrElse i
                (ops reduceLeft (_ | _)) ~ with_sinds_ss(j + 1){ od ⇒
                  val d = od getOrElse j
                  binOpR(name, ops, l, r, f)(StrictIndent(d))
                }
              }
            case InBrace ⇒
              l(InBrace) ~ (
                (whitespace.+ ~> (ops reduceLeft (_ | _)) <~ whitespace.+) ~ binOpR(name, ops, l, r, f)(InBrace)
                  |             ((ops reduceLeft (_ | _)) <~ whitespace.*) ~ binOpR(name, ops, l, r, f)(InBrace)
              )
            case OneLine ⇒
              l(OneLine) ~ (
                (rep1(" ") ~> (ops reduceLeft (_ | _)) <~ rep1(" ")) ~ binOpR(name, ops, l, r, f)(OneLine)
                  |          ((ops reduceLeft (_ | _)) <~ rep (" ")) ~ binOpR(name, ops, l, r, f)(OneLine)
              )
          }
        ) ^^ {
          case l ~ (op ~ r) ⇒ f(op, l, r)
        }
      ) | r(i)
    )

  def tupleInner(i: Indent): ParserExt[List[RabbitTree]] = ("tupleInner", i) nameParserExt (
    i match {
      case StrictIndent(i) ⇒
        fnCall(StrictIndent(i)) ~ rep( 
          rep(" ") ~> "," ~> ("tupleInner-1", StrictIndent(i)).nameParserExt(with_sinds(i){ j ⇒
            fnCall(StrictIndent(j))
          }
        )) ^^ {
          case f ~ l => f :: l
        }
      case InBrace ⇒
        repsep(fnCall(InBrace), whitespace.* ~ "," ~ whitespace.*)
      case OneLine ⇒
        repsep(fnCall(OneLine), rep(" ") ~ "," ~ rep(" "))
    }
  )

  def fnCall(i: Indent): ParserExt[RabbitTree] = noNameParserExt(
    (
      ("fnCall", i) nameParserExt (
        i match {
          case StrictIndent(i) ⇒
            term(StrictIndent(i)) ~ (
                "(" ~> whitespace.* ~> tupleInner(InBrace) <~ whitespace.* <~ ")"
              | with_sinds_ss(i + 1){
                  case Some(j) =>
                    tupleInner(StrictIndent(j))
                  case None =>
                    tupleInner(OneLine)
                }
            )
          case InBrace ⇒
            term(InBrace) ~ (
                "(" ~> whitespace.* ~> tupleInner(InBrace) <~ whitespace.* <~ ")"
              | whitespace.+ ~> tupleInner(InBrace)
            )
          case OneLine ⇒
            term(OneLine) ~ (
                "(" ~> rep(" ") ~> tupleInner(OneLine) <~ rep(" ") <~ ")"
              | rep1(" ") ~> tupleInner(OneLine)
            )
        }
      ) ^^ {
        case f ~ x ⇒ FnCallTree(f, x)
      }
    ) | varRef(i)
  )

  def expr1 = binOpL("expr1", List("==", "!=", "<", "<=", ">", ">="), expr2, expr2, BinaryOpTree) _

  def expr2 = binOpL("expr2", List("+", "-"), expr3, expr3, BinaryOpTree) _

  def expr3 = binOpL("expr3", List("*", "//", "/", "%%", "%"), expr4, expr4, BinaryOpTree) _

  def expr4 = binOpR("expr4", List("**"), unOp, unOp, BinaryOpTree) _

  def unOp(i: Indent): ParserExt[RabbitTree] = ("unOp", i) nameParserExt (
    ("+" | "-" | "*").? ~ term(i) ^^ {
      case Some(op) ~ v ⇒ UnaryOpTree(op, v)
      case None ~ v ⇒ v
    }
  )

  def term(i: Indent): ParserExt[RabbitTree] = ("term", i) nameParserExt (
      num
    | string
    | bool
    | "(" ~ whitespace.* ~> stmt(InBrace) <~ whitespace.* ~ ")"
    | varRef(i)
//  | failure("no term found")
  )

  def varDef(i: Indent): ParserExt[VarDefTree] = ("varDef", i) nameParserExt (
    i match {
      case StrictIndent(i) ⇒
        pattern ~ with_sindps(i + 1){ oj ⇒
          val j = oj getOrElse i
          ":=" ~ sindps(j + 1)
        } ~ assign(StrictIndent(i))
      case InBrace ⇒
        pattern ~ (whitespace.* ~ ":=" ~ whitespace.*) ~ assign(InBrace)
      case OneLine ⇒
        pattern ~ (rep(" ") ~ ":=" ~ rep(" ")) ~ assign(OneLine)
    }
  ) ^^ {
    case p ~ _ ~ v ⇒ VarDefTree(p, v)
  }

  def condStmt(i: Indent): ParserExt[CondTree] = ("condStmt", i) nameParserExt (
    i match {
      case StrictIndent(i) ⇒
        ("if" | "unless") ~ (rep1(" ") ~> fnCall(OneLine)) ~ with_sindps(i + 1){
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
        ("if" | "unless") ~ (rep1(" ") ~> fnCall(OneLine)) ~ with_sindps(1){
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
        ("if" | "unless") ~ (rep1(" ") ~> fnCall(OneLine))
        ~ (rep1(" ") ~ "then" ~ rep1(" ") ~> stmt(OneLine))
        ~ (rep1(" ") ~ "else" ~ rep1(" ") ~> stmt(OneLine)).?
      )
    }
  ) ^^ {
    case "if"     ~ i ~ t ~ e ⇒ IfTree(i, t, e)
    case "unless" ~ u ~ t ~ e ⇒ UnlessTree(u, t, e)
  }

  def loopStmt(i: Indent): ParserExt[LoopTree] = noNameParserExt(whileStmt(i) | forStmt(i))

  def whileStmt(i: Indent): ParserExt[LoopTree] = ("whileStmt", i) nameParserExt (
    i match {
      case StrictIndent(i) ⇒
        ("while" | "until") ~ (rep1(" ") ~> fnCall(OneLine)) ~ with_sindps(i + 1){
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
        ("while" | "until") ~ (rep1(" ") ~> fnCall(OneLine)) ~ with_sindps(1){
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
        ("while" | "until") ~ (rep1(" ") ~> fnCall(OneLine))
        ~ (rep1(" ") ~ "do" ~ rep1(" ") ~> stmt(OneLine))
        ~ (rep1(" ") ~ "else" ~ rep1(" ") ~> stmt(OneLine)).?
      )
    }
  ) ^^ {
    case "while" ~ w ~ d ~ e ⇒ WhileTree(w, d, e)
    case "until" ~ u ~ d ~ e ⇒ UntilTree(u, d, e)
  }

  def forStmt(i: Indent): ParserExt[LoopTree] = ("forStmt", i) nameParserExt (
    i match {
      case StrictIndent(i) ⇒
        "for" ~> rep1(" ") ~> pattern ~ (rep1(" ") ~ "in" ~ rep(" ") ~> fnCall(OneLine)) ~ with_sindps(i + 1){
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
        "for" ~> rep1(" ") ~> pattern ~ (rep1(" ") ~ "in" ~ rep(" ") ~> fnCall(OneLine)) ~ with_sindps(1){
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

  def block(i: Indent): ParserExt[BlockTree] = ("block", i) nameParserExt (
    i match {
      case StrictIndent(i) ⇒ stmt(StrictIndent(i)).? ~ rep(with_sind(i)(i ⇒ stmt(StrictIndent(i)).?))
      case InBrace ⇒ stmt(InBrace).? ~ rep(whitespace.* ~ ";" ~ whitespace.* ~> stmt(InBrace).?)
      case OneLine ⇒ stmt(InBrace).? ~ rep(rep(" ") ~ ";" ~ rep(" ") ~> stmt(InBrace).?)
    }
  ) ^^ {case x ~ xs ⇒ BlockTree((x :: xs) collect {case Some(x) ⇒ x})}

  def parse(s: String) = parseAll(block(StrictIndent(0)), s)
}
