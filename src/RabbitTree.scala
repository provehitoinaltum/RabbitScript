package org.rabbitscript.trees

sealed trait RabbitTree {
  def debugJavaScript: String
}
abstract class ValueTree extends RabbitTree
case class IntTree(value: Int) extends ValueTree {
  def debugJavaScript = value.toString()
}
case class FloatTree(value: Double) extends ValueTree {
  def debugJavaScript = value.toString()
}
case class StringTree(quote: String)(value: String) extends ValueTree {
  lazy val debugJavaScript = {
    val s = value.replace("\\", "\\\\")
                 .replace("\t", "\\t")
                 .replace("\n", "\\n")
                 .replace("\r", "\\r")
                 .replace("\0", "\\0")
                 .replace(quote, s"\\$quote")
    quote + (
      "[\u0000-\u001f]".r replaceAllIn (s, m => "\\\\u%04x" format m.toString()(0).toInt)
    ) + quote
  }
}
case class VarDefTree(name: String, value: RabbitTree) extends RabbitTree {
  def debugJavaScript = s"var $name;\n$name = ${value.debugJavaScript}"
}
case class VarRefTree(name: String) extends RabbitTree {
  def debugJavaScript = name
}
case class UnaryOpTree(op: String, v: RabbitTree) extends RabbitTree {
  def debugJavaScript = {
    val vs = v match {
      case _: ValueTree => v.debugJavaScript
      case _ => "(" + v.debugJavaScript + ")"
    }
    op + vs
  }
}
case class BinaryOpTree(op: String, l: RabbitTree, r: RabbitTree) extends RabbitTree {
  def debugJavaScript = {
    val ls = l match {
      case _: ValueTree => l.debugJavaScript
      case _: VarRefTree => l.debugJavaScript
      case _ => "(" + l.debugJavaScript + ")"
    }
    val rs = r match {
      case _: ValueTree => r.debugJavaScript
      case _: VarRefTree => r.debugJavaScript
      case _ => "(" + r.debugJavaScript + ")"
    }
    ls + " " + op + " " + rs
  }
}
case class BlockTree(stmts: List[RabbitTree]) extends RabbitTree {
  def debugJavaScript = 
    (stmts map (_.debugJavaScript) mkString ";\n") + ";"
}
