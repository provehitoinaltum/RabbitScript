package org.rabbitscript
import util.parsing.combinator._

object RabbitParser extends RegexParsers {
  import trees._
  def expr: Parser[RabbitTree] = expr1
  def expr1: Parser[RabbitTree] = expr2
  def expr2: Parser[RabbitTree] = expr3
  def expr3: Parser[RabbitTree] = expr4
  def expr4: Parser[RabbitTree] = expr5
  def expr5: Parser[RabbitTree] = expr6
  def expr6: Parser[RabbitTree] = term
  def term: Parser[RabbitTree] = num
  def num: Parser[RabbitTree] = """-?+[1-9][0-9]*+|0""".r ~ """\.[0-9]++""".r.? ^^ {
    case intPart ~ Some(decPart) => new FloatTree((intPart + decPart).toDouble)
    case intPart ~ None => new IntTree(intPart.toInt)
  }
  def parse(s: String) = parseAll(expr, s)
}
