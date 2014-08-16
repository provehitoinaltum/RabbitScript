package org.rabbitscript.trees
import net.akouryy.common._
import lib._

sealed trait RabbitTree {
  def debugJavaScript: String
  def dumpTree(): Unit
}
abstract class RabbitLeaf extends RabbitTree
case class IntNode(value: Int) extends RabbitLeaf {
  def debugJavaScript = value.toString()
  def dumpTree() { println(s"IntNode $value") }
}
case class FloatNode(value: Double) extends RabbitLeaf {
  def debugJavaScript = value.toString()
  def dumpTree() { println(s"FloatNode $value") }
}
case class StringNode(quote: String)(value: String) extends RabbitLeaf {
  def debugJavaScript = s
  private lazy val s = {
    val s =
      value.replaceWithTuples(
        "\\" → "\\\\",
        "\t" → "\\t",
        "\n" → "\\n",
        "\r" → "\\r",
        "\u0000" → "\\0",
        quote → s"\\$quote"
      )
    quote + (
      "[\u0000-\u001f]".r replaceAllIn (s, m ⇒ "\\\\u%04x" format m.toString()(0).toInt)
    ) + quote
  }
  override def toString = s"StringNode($s)"
  def dumpTree() { println(s"StringNode $value") }
}
case class BooleanNode(value: Boolean) extends RabbitLeaf {
  def debugJavaScript = if(value) "true" else "false"
  def dumpTree() { println(s"BooleanNode $value") }
}
trait PatternTree extends RabbitTree
case class TypePatternTree(_type: String, pattern: PatternTree) extends RabbitLeaf with PatternTree {
  def debugJavaScript = pattern.debugJavaScript
  def dumpTree() {
    println(s"TypePatternTree ${_type}")
    AdvancedPrintStream.increaseIndent()
    pattern.dumpTree()
    AdvancedPrintStream.decreaseIndent()
  }
}
case class VarPatternNode(name: String) extends RabbitLeaf with PatternTree {
  def debugJavaScript = name
  def dumpTree() { println(s"VarPatternNode $name") }
}
case class TuplePatternTree(ps: List[PatternTree]) extends RabbitTree with PatternTree {
  def debugJavaScript = "(" + (ps map (_.debugJavaScript) mkString ", ") + ")"
  def dumpTree() {
    println("TuplePatternTree")
    AdvancedPrintStream.increaseIndent()
    ps.foreach(_.dumpTree())
    AdvancedPrintStream.decreaseIndent()
  }
}
case class VarRefNode(name: String) extends RabbitLeaf {
  def debugJavaScript = name
  def dumpTree() { println(s"VarRefNode $name") }
}
case class UnaryOpTree(op: String, v: RabbitTree) extends RabbitTree {
  def debugJavaScript = {
    op + (
      v match {
        case _: RabbitLeaf ⇒ v.debugJavaScript
        case _ ⇒ "(" + v.debugJavaScript + ")"
      }
    )
  }
  def dumpTree() {
    println(s"UnaryOpTree $op")
    AdvancedPrintStream.increaseIndent()
    v.dumpTree()
    AdvancedPrintStream.decreaseIndent()
  }
}
case class BinaryOpTree(op: String, l: RabbitTree, r: RabbitTree) extends RabbitTree {
  def debugJavaScript = {
    (
      l match {
        case _: RabbitLeaf ⇒ l.debugJavaScript
        case _ ⇒ "(" + l.debugJavaScript + ")"
      }
    ) + " " + op + " " + (
      r match {
        case _: RabbitLeaf ⇒ r.debugJavaScript
        case _ ⇒ "(" + r.debugJavaScript + ")"
      }
    )
  }
  def dumpTree() {
    println(s"BinaryOpTree $op")
    AdvancedPrintStream.increaseIndent()
    l.dumpTree()
    r.dumpTree()
    AdvancedPrintStream.decreaseIndent()
  }
}
case class AssignTree(name: VarRefNode, value: RabbitTree) extends RabbitTree {
  def debugJavaScript = name.debugJavaScript + " = " + value.debugJavaScript
  def dumpTree() {
    println(s"AssignTree ${name.name}")
    AdvancedPrintStream.increaseIndent()
    value.dumpTree()
    AdvancedPrintStream.decreaseIndent()
  }
}
case class VarDefTree(pattern: PatternTree, value: RabbitTree) extends RabbitTree {
  def debugJavaScript =
    s"var " + pattern.debugJavaScript + " = " + value.debugJavaScript
  def dumpTree() {
    println(s"VarDefTree")
    AdvancedPrintStream.increaseIndent()
    pattern.dumpTree()
    value.dumpTree()
    AdvancedPrintStream.decreaseIndent()
  }
}
abstract class CondTree extends RabbitTree
case class IfTree(cond: RabbitTree, _if: RabbitTree, _else: Option[RabbitTree]) extends CondTree {
  def debugJavaScript =
    s"if(${cond.debugJavaScript}){\n${_if.debugJavaScript}\n}" + (
      _else match {
        case Some(e) ⇒ s"else{\n${e.debugJavaScript}\n}"
        case None ⇒ ""
      }
    )
  def dumpTree() {
    println(s"IfTree")
    AdvancedPrintStream.increaseIndent()
    cond.dumpTree()
    _if.dumpTree()
    _else map (_.dumpTree())
    AdvancedPrintStream.decreaseIndent()
  }
}
case class UnlessTree(cond: RabbitTree, _unless: RabbitTree, _else: Option[RabbitTree]) extends CondTree {
  def debugJavaScript =
    s"if(!(${cond.debugJavaScript}\n${_unless.debugJavaScript}\n}" + (
      _else match {
        case Some(e) ⇒ s"else{\n${e.debugJavaScript}\n}"
        case None ⇒ ""
      }
    )
  def dumpTree() {
    println(s"UnlessTree")
    AdvancedPrintStream.increaseIndent()
    cond.dumpTree()
    _unless.dumpTree()
    _else map (_.dumpTree())
    AdvancedPrintStream.decreaseIndent()
  }
}
abstract class LoopTree extends RabbitTree
case class WhileTree(cond: RabbitTree, _while: RabbitTree, _else: Option[RabbitTree]) extends LoopTree {
  def debugJavaScript =
    _else match {
      case Some(e) ⇒
        s"var __break=false;\nwhile(${cond.debugJavaScript}){\n${_while.debugJavaScript}\n}" +
        s"if(!__break){\n${e.debugJavaScript}\n}"
      case None ⇒
        s"while(${cond.debugJavaScript}){\n${_while.debugJavaScript}\n}"
    }
  def dumpTree() {
    println(s"WhileTree")
    AdvancedPrintStream.increaseIndent()
    cond.dumpTree()
    _while.dumpTree()
    _else map (_.dumpTree())
    AdvancedPrintStream.decreaseIndent()
  }
}
case class UntilTree(cond: RabbitTree, _until: RabbitTree, _else: Option[RabbitTree]) extends LoopTree {
  def debugJavaScript =
    _else match {
      case Some(e) ⇒
        s"var __break=false;\nwhile(!(${cond.debugJavaScript})){\n${_until.debugJavaScript}\n}" +
        s"if(!__break){\n${e.debugJavaScript}\n}"
      case None ⇒
        s"while(!(${cond.debugJavaScript})){\n${_until.debugJavaScript}\n}"
    }
  def dumpTree() {
    println(s"UntilTree")
    AdvancedPrintStream.increaseIndent()
    cond.dumpTree()
    _until.dumpTree()
    _else map (_.dumpTree())
    AdvancedPrintStream.decreaseIndent()
  }
}
case class ForTree(pattern: PatternTree, expr: RabbitTree, block: RabbitTree) extends LoopTree {
  def debugJavaScript =
    s"for(${pattern.debugJavaScript} in " + (
      expr match {
        case _: RabbitLeaf ⇒ expr.debugJavaScript
        case _ ⇒ "(" + expr.debugJavaScript + ")"
      }
    ) + s"){\n${block.debugJavaScript}\n}"
  def dumpTree() {
    println(s"ForTree")
    AdvancedPrintStream.increaseIndent()
    pattern.dumpTree()
    expr.dumpTree()
    block.dumpTree()
    AdvancedPrintStream.decreaseIndent()
  }
}
case class BlockTree(stmts: List[RabbitTree]) extends RabbitTree {
  def debugJavaScript = ((stmts map (x ⇒ x.debugJavaScript) mkString ";\n") mapLines ("  " + _)) + ";"
  def dumpTree() {
    println(s"BlockTree")
    AdvancedPrintStream.increaseIndent()
    stmts.foreach(_.dumpTree())
    AdvancedPrintStream.decreaseIndent()
  }
}
case class FnCallTree(f: RabbitTree, args: List[RabbitTree]) extends RabbitTree {
  def debugJavaScript = f.debugJavaScript + "(" + (args map (_.debugJavaScript) mkString ",") + ")"
  def dumpTree() {
    AdvancedPrintStream.print(s"FnCallTree ")
    AdvancedPrintStream.increaseIndent()
    f.dumpTree()
    args.foreach(_.dumpTree())
    AdvancedPrintStream.decreaseIndent()
  }
}
