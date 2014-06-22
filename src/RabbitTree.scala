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
  def debugJavaScript = s
  private lazy val s = {
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
case class VarRefTree(name: String) extends RabbitTree {
  def debugJavaScript = name
}
case class UnaryOpTree(op: String, v: RabbitTree) extends RabbitTree {
  def debugJavaScript = {
    op + (
      v match {
        case _: ValueTree => v.debugJavaScript
        case _ => "(" + v.debugJavaScript + ")"
      }
    )
  }
}
case class BinaryOpTree(op: String, l: RabbitTree, r: RabbitTree) extends RabbitTree {
  def debugJavaScript = {
    (
      l match {
        case _: ValueTree => l.debugJavaScript
        case _: VarRefTree => l.debugJavaScript
        case _ => "(" + l.debugJavaScript + ")"
      }
    ) + " " + op + " " + (
      r match {
        case _: ValueTree => r.debugJavaScript
        case _: VarRefTree => r.debugJavaScript
        case _ => "(" + r.debugJavaScript + ")"
      }
    )
  }
}
case class VarDefTree(name: String, value: RabbitTree) extends RabbitTree {
  def debugJavaScript = s"var $name;\n$name = ${value.debugJavaScript}"
}
abstract class CondTree extends RabbitTree
case class IfTree(cond: RabbitTree, _if: RabbitTree, _else: Option[RabbitTree]) extends CondTree {
  def debugJavaScript =
    "if(" + cond.debugJavaScript + "){\n" + _if.debugJavaScript + "\n}" + (
      _else match {
        case Some(e: CondTree) => "else " + e.debugJavaScript
        case Some(e) => "else{\n" + e.debugJavaScript + "\n}"
        case None => ""
      }
    )
}
case class UnlessTree(cond: RabbitTree, _unless: RabbitTree, _else: Option[RabbitTree]) extends CondTree {
  def debugJavaScript =
    "if(!(" + cond.debugJavaScript + ")){\n" + _unless.debugJavaScript + "\n}" + (
      _else match {
        case Some(e: CondTree) => "else " + e.debugJavaScript
        case Some(e) => "else{\n" + e.debugJavaScript + "\n}"
        case None => ""
      }
    )
}
abstract class LoopTree extends RabbitTree
case class WhileTree(cond: RabbitTree, _while: RabbitTree, _else: Option[RabbitTree]) extends LoopTree {
  def debugJavaScript =
    _else match {
      case Some(e) => (
          "var _break=false;\nwhile(" + cond.debugJavaScript + "){\n" + _while.debugJavaScript + "\n}\n"
        + "if(_break){\n" + e.debugJavaScript + "\n}"
        )
      case None =>
        "while(" + cond.debugJavaScript + "){\n" + _while.debugJavaScript + "\n}"
    }
}
case class UntilTree(cond: RabbitTree, _until: RabbitTree, _else: Option[RabbitTree]) extends LoopTree {
  def debugJavaScript =
    _else match {
      case Some(e) => (
          "var __break=false;\n"
        + "while(!(" + cond.debugJavaScript + "))" + "{\n" + _until.debugJavaScript + "\n}\n"
        + "if(!__break){\n" + e.debugJavaScript + "\n}"
        )
      case None =>
        "while(!(" + cond.debugJavaScript + "))" + "{\n" + _until.debugJavaScript + "\n}"
    }
}
case class ForTree(_var: String, expr: RabbitTree, block: RabbitTree) extends LoopTree {
  def debugJavaScript =
    "for(" + _var + " in " + (
      expr match {
        case _: ValueTree => expr.debugJavaScript
        case _: VarRefTree => expr.debugJavaScript
        case _ => "(" + expr.debugJavaScript + ")"
      }
    ) + "){\n" + block.debugJavaScript + "\n}"
}
case class BlockTree(stmts: List[RabbitTree]) extends RabbitTree {
  def debugJavaScript = 
    ((stmts map (x => x.debugJavaScript) mkString ";\n").lines map ("  " + _) mkString "\n") + ";"
}
