package org.rabbitscript.trees

sealed trait RabbitTree {
  def toJavaScript: String
}
abstract class ValueTree extends RabbitTree
case class IntTree(value: Int) extends ValueTree{
  override def toJavaScript = value.toString()
}
case class FloatTree(value: Double) extends ValueTree{
  override def toJavaScript = value.toString()
}
