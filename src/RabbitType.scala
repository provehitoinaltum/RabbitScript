package org.rabbitscript
import trees._
import net.akouryy.common.lib._

import collection.mutable

trait RabbitTypeLike {
  val superType: RabbitTypeLike
  def <(t: RabbitType) = superType <= t
  def <=(t: RabbitType) = this == t || this < t
  def >(t: RabbitType) = t < this
  def >=(t: RabbitType) = t <= this
  def apply(name: String): Option[RabbitType]
}
class RabbitType(val name: String, val superType: RabbitType) extends RabbitTypeLike with mutable.MapLike[String, RabbitType] {
  protected[this] val members: mutable.Map[String, RabbitType]
  def update(name: String, type: RabbitType) = members(name) = type
  def apply(name: String) = members get name orElse superType(name)
}
object RabbitBaseType extends RabbitType("Base", null) {
  override def <(t: RabbitType) = false
  override def apply(name: String) = members get name
}
class RabbitParametricType(val superType: RabbitType) extends RabbitTypeLike {
  def apply(name: String) = superType(name)
}
class RabbitTypeGenerator(val name: String, arity: Int, superTypes: Map[Int, RabbitType] = Map()) extends List[RabbitType] ⇒ RabbitGenericType {
  class RabbitGenericType private[RabbitTypeGenerator](val args: Map[RabbitType, RabbitType]) extends RabbitTypeLike {
    def apply(name: String) = members
  }
  require(arity >= 0)
  val paramTypes = List.tabulate(arity){i ⇒ new RabbitParametricType(superTypes getOrElse (i, RabbitType.Object))}
  protected[this] val members: mutable.Map[String, RabbitType]
  def update(name: String, type: RabbitType) = members(name) = type
  def apply(args: List[RabbitType]) = {
    require(args.length == arity)
    new RabbitGenericType(this, args)
  }
}

object RabbitType {
  def unapply(t: RabbitType) = Some(t.name)

  val Object = new RabbitType("Object", null)
  val Bool = new RabbitType("Boolean", Object)
  val Number = new RabbitType("Number", Object)
  val Int = new RabbitType("Int", Number)
  val Float = new RabbitType("Float", Number)
  val String = new RabbitType("String", Object)

  val Tuple2 = new RabbitTypeGenerator("Tuple", 2)
  val Tuple3 = new RabbitTypeGenerator("Tuple", 3)
  val Tuple4 = new RabbitTypeGenerator("Tuple", 4)
  val Tuple5 = new RabbitTypeGenerator("Tuple", 5)

  val Function0 = new RabbitTypeGenerator("Function", 1)
  val Function1 = new RabbitTypeGenerator("Function", 2)
  val Function2 = new RabbitTypeGenerator("Function", 3)
  val Function3 = new RabbitTypeGenerator("Function", 4)
  val Function4 = new RabbitTypeGenerator("Function", 5)
  val Function5 = new RabbitTypeGenerator("Function", 6)

  Object("==") = Function()
}

object RabbitTyper {
  class Environment(parent: Option[Environment]) {
    val vars = mutable.Map[String, RabbitType]()
    def apply(s: String): Option[RabbitType] = vars get s orElse parent.flatMap(_ apply s)
  }
  val env = new Environment(None)
  def checkType(program: RabbitTree) = program match {
    case IntNode(_) ⇒                  Some(RabbitType.Int)
    case FloatNode(_) ⇒                Some(RabbitType.Float)
    case StringNode(_)(_) ⇒            Some(RabbitType.String)
    case BooleanNode(_) ⇒              Some(RabbitType.Bool)
    case TypePatternTree(typ, pat) ⇒   checkType(pat) match {
                                          case Some(_: RabbitParametricType) ⇒
                                            Some(typ)
                                          case Some(t: RabbitType) ⇒
                                            if(typ <= t) Some(typ)
                                          case None ⇒ None
                                        }
    case VarPatternNode(_) ⇒           Some(RabbitParametricType)
    case TuplePatternTree(pts) ⇒       (pts @unchecked) match {
                                          case List(_, _)          ⇒ Tuple2(pts)
                                          case List(_, _, _)       ⇒ Tuple3(pts)
                                          case List(_, _, _, _)    ⇒ Tuple4(pts)
                                          case List(_, _, _, _, _) ⇒ Tuple5(pts)
                                        }
    case VarRefNode(name: String) ⇒    env(name)
    case UnaryOpTree(op, v) ⇒          ;
    case BinaryOpTree(op, l, r) ⇒      ;
    case AssignTree(n, v) ⇒            checkType(pat) match { // TODO
                                          case Some(_: RabbitParametricType) ⇒
                                            Some(typ)
                                          case Some(s: RabbitType) ⇒
                                            if(typ <= checkType(pat)) Some(typ)
                                          case None ⇒ None
                                        }env(n)
    case VarDefTree(pattern: PatternTree, value: RabbitTree) ⇒
    case IfTree(cond: RabbitTree, _if: RabbitTree, _else: Option[RabbitTree]) ⇒
    case UnlessTree(cond: RabbitTree, _unless: RabbitTree, _else: Option[RabbitTree]) ⇒
    case WhileTree(cond: RabbitTree, _while: RabbitTree, _else: Option[RabbitTree]) ⇒
    case UntilTree(cond: RabbitTree, _until: RabbitTree, _else: Option[RabbitTree]) ⇒
    case ForTree(pattern: PatternTree, expr: RabbitTree, block: RabbitTree) ⇒
    case BlockTree(stmts: List[RabbitTree]) ⇒
    case FnCallTree(f: RabbitTree, args: List[RabbitTree]) ⇒
  }
}
