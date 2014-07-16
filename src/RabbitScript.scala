package org.rabbitscript
import net.akouryy.common.Lib._

import collection._

object RabbitScript {
  def main(args: Array[String]) {
    val cla = new CommandLineArgument[Unit]
    cla("v") = CommandLineOption (
      arity = 0,
      f = { _ ⇒ println("RabbitScript ver. α33\n") }
    )
    cla("h") = CommandLineOption (
      arity = 0,
      f = { _ ⇒
        println("""Is the order help?
                  |Usage: rsc <options> [<filename>]
                  |
                  |<options>:
                  |  -v      show version number
                  |  -h      show this help message
                  |""".stripMargin)
      }
    )
    cla.main = ( args ⇒
      using(io.Source fromFile args(0)) {
        val rp = new RabbitParser
        rp parse _.getLines.mkString("\n") match {
          case rp.Success(tree, _) ⇒
            println(tree)
            tree.debugJavaScript eachLine (l ⇒ println(l drop 2))
          case x ⇒
            println(x)
        }
      }
    )
    cla parse args.toList
  }
}

case class CommandLineOption(arity: Int, f: List[String] ⇒ Unit)

class CommandLineArgument[T] {
  private val options = mutable.Map[String, CommandLineOption]()
  private var _main: List[String] ⇒ T = _
  def update(opt :String, clo: CommandLineOption) {
    options(opt) = clo
  }
  def main{}
  def main_=(main: List[String] ⇒ T) {
    _main = main
  }
  def parse(args: List[String]): Either[String, Option[T]] = args match {
    case init :: tail ⇒
      if(init(0) == '-')
        options.get(init substring 1) match {
          case Some(CommandLineOption(arity, f)) ⇒
            val (take, drop) = tail splitAt arity
            f(take)
            parse(drop)
          case None ⇒
            Left("bad option: " + init)
        }
      else
        Right(Some(_main(args)))
    case Nil ⇒
      Right(None)
  }
}
