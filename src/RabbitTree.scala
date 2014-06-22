package org.rabbitscript.trees

sealed trait RabbitTree {
  def debugJavaScript: String
}
abstract class RabbitLeaf extends RabbitTree
case class IntNode(value: Int) extends RabbitLeaf {
  def debugJavaScript = value.toString()
}
case class FloatNode(value: Double) extends RabbitLeaf {
  def debugJavaScript = value.toString()
}
case class StringNode(quote: String)(value: String) extends RabbitLeaf {
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
case class BooleanNode(value: Boolean) extends RabbitLeaf {
  def debugJavaScript = if(value) "true" else "false"
}
trait PatternTree extends RabbitTree
case class TypePatternTree(_type: String, pattern: PatternTree) extends RabbitLeaf with PatternTree {
  def debugJavaScript = pattern.debugJavaScript
}
case class VarPatternNode(name: String) extends RabbitLeaf with PatternTree {
  def debugJavaScript = name
}
case class TuplePatternTree(ps: List[PatternTree]) extends RabbitTree with PatternTree {
  def debugJavaScript = "(" + (ps map (_.debugJavaScript) mkString ", ") + ")"
}
case class VarRefNode(name: String) extends RabbitLeaf {
  def debugJavaScript = name
}
case class UnaryOpTree(op: String, v: RabbitTree) extends RabbitTree {
  def debugJavaScript = {
    op + (
      v match {
        case _: RabbitLeaf => v.debugJavaScript
        case _ => "(" + v.debugJavaScript + ")"
      }
    )
  }
}
case class BinaryOpTree(op: String, l: RabbitTree, r: RabbitTree) extends RabbitTree {
  def debugJavaScript = {
    (
      l match {
        case _: RabbitLeaf => l.debugJavaScript
        case _ => "(" + l.debugJavaScript + ")"
      }
    ) + " " + op + " " + (
      r match {
        case _: RabbitLeaf => r.debugJavaScript
        case _ => "(" + r.debugJavaScript + ")"
      }
    )
  }
}
case class VarDefTree(pattern: PatternTree, value: RabbitTree) extends RabbitTree {
  def debugJavaScript =
    s"var " + pattern.debugJavaScript + ";\n" + pattern.debugJavaScript + " = " + value.debugJavaScript
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
case class UnlessTree(cond: RabbitTree, unless: RabbitTree, _else: Option[RabbitTree]) extends CondTree {
  def debugJavaScript =
    "if(!(" + cond.debugJavaScript + ")){\n" + unless.debugJavaScript + "\n}" + (
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
          "var __break=false;\nwhile(" + cond.debugJavaScript + "){\n" + _while.debugJavaScript + "\n}\n"
        + "if(__break){\n" + e.debugJavaScript + "\n}"
        )
      case None =>
        "while(" + cond.debugJavaScript + "){\n" + _while.debugJavaScript + "\n}"
    }
}
case class UntilTree(cond: RabbitTree, until: RabbitTree, _else: Option[RabbitTree]) extends LoopTree {
  def debugJavaScript =
    _else match {
      case Some(e) => (
          "var __break=false;\n"
        + "while(!(" + cond.debugJavaScript + "))" + "{\n" + until.debugJavaScript + "\n}\n"
        + "if(!__break){\n" + e.debugJavaScript + "\n}"
        )
      case None =>
        "while(!(" + cond.debugJavaScript + "))" + "{\n" + until.debugJavaScript + "\n}"
    }
}
case class ForTree(pattern: PatternTree, expr: RabbitTree, block: RabbitTree) extends LoopTree {
  def debugJavaScript =
    "for(" + pattern.debugJavaScript + " in " + (
      expr match {
        case _: RabbitLeaf => expr.debugJavaScript
        case _ => "(" + expr.debugJavaScript + ")"
      }
    ) + "){\n" + block.debugJavaScript + "\n}"
}
case class BlockTree(stmts: List[RabbitTree]) extends RabbitTree {
  def debugJavaScript = 
    ((stmts map (x => x.debugJavaScript) mkString ";\n").lines map ("  " + _) mkString "\n") + ";"
}
