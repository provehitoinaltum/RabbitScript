package org.rabbitscript
import util.parsing.combinator._

class RabbitParser() extends RegexParsers {
  def expr: Parser[Any] = expr1
  def expr1: Parser[Any] = expr2
  def expr2: Parser[Any] = expr3
  def expr3: Parser[Any] = expr4
  def expr4: Parser[Any] = expr5
  def expr5: Parser[Any] = expr6
  def expr6: Parser[Any] = term
  def term: Parser[Any] = num
  def num: Parser[Any] = """-?[1-9][0-9]*+(?:\.[0-9]++)?|0""".r
  def parse(s: String) = parseAll(expr, s)
}
