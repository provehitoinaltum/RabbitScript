package org.rabbitscript.trees

sealed trait RabbitTree {
  def toJavaScript: String
}
abstract class ValueTree extends RabbitTree
case class IntTree(value: Int) extends ValueTree {
  override def toJavaScript = value.toString()
}
case class FloatTree(value: Double) extends ValueTree {
  override def toJavaScript = value.toString()
}
case class StringTree(quote: String)(value: String) extends ValueTree {
  override def toJavaScript = js
  lazy val js = {
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
  override def toJavaScript = s"var $name;\n$name = ${value.toJavaScript}"
}
case class TwoOpTree(op: String, l: RabbitTree, r: RabbitTree) extends RabbitTree {
  override def toJavaScript = s"(${l.toJavaScript}) $op (${r.toJavaScript})"
}
