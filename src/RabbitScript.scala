package org.rabbitscript
import net.akouryy.common.Lib._

import collection._

object RabbitScript {
  def main(args: Array[String]) {
    val cla = new CommandLineArgument[Unit]
    var warningLevel = 1
    cla("w") = CommandLineOption (
      arity = 1,
      f = {
        case w :: Nil ⇒
          try {
            val warning = w.toInt
            if(0 <= warning) {
              if(warning <= 3){
                warningLevel = warning
                Right()
              } else {
                Left("warning level must be <= 3")
              }
            } else {
              Left("warning level must be >= 0")
            }
          } catch {
            case _: NumberFormatException ⇒
              Left(s"can't convert $w to integer")
          }
      }
    )
    cla("v") = CommandLineOption (
      arity = 0,
      f = { case Nil ⇒ println("RabbitScript ver. α33\n"); Right() }
    )
    cla("h") = CommandLineOption (
      arity = 0,
      f = {
        case Nil ⇒
          println("""Is the order help?
                    |Usage: rabbit <options> [<filename>]
                    |
                    |<options>:
                    |  -w [0-3]      set warning level
                    |  
                    |  -v            show version number
                    |  -h            show this help message
                    |""".stripMargin)
          Right()
      }
    )
    cla.main = { args ⇒
      try {
        (io.Source fromFile args(0)) →→ { sc ⇒
          val rp = new RabbitParser
          rp parse sc.getLines.mkString("\n") match {
            case rp.Success(tree, _) ⇒
              println(tree)
              tree.debugJavaScript eachLine (l ⇒ println(l drop 2))
            case x ⇒
              println(x)
          }
        }
        Right()
      } catch {
        case _: java.io.FileNotFoundException ⇒
          Left(s"file ${args(0)} not found")
      }
    }
    cla parse args.toList match {
      case Left(s) ⇒
        Console.err println s
        Console.err println "try -h for more information."
      case Right(Some(_)) ⇒
      case Right(None) ⇒ println("no file given")
    }
  }
}

case class CommandLineOption(arity: Int, f: PartialFunction[List[String], Either[String, Unit]])

class CommandLineArgument[A] {
  private val options = mutable.Map[String, CommandLineOption]()
  private var _main: List[String] ⇒ Either[String, A] = _
  def update(opt :String, clo: CommandLineOption) {
    options(opt) = clo
  }
  def main{}
  def main_=(main: List[String] ⇒ Either[String, A]) {
    _main = main
  }
  def parse(args: List[String]): Either[String, Option[A]] = args match {
    case init :: tail ⇒
      if(init(0) == '-')
        options.get(init substring 1) match {
          case Some(CommandLineOption(arity, f)) ⇒
            val (take, drop) = tail splitAt arity
            val l = take.length
            if(l == arity)
              f(take) match {
                case Left(s) ⇒ Left(s"something wrong in $init: $s")
                case Right(()) ⇒ parse(drop)
              }
            else
              Left(s"bad argument length: $l for $arity")
          case None ⇒
            Left(s"bad option: $init")
        }
      else
        _main(args) match {
          case Left(s) ⇒ Left(s"something wrong in main arguments: $s")
          case Right(x) ⇒ Right(Some(x))
        }
    case Nil ⇒
      Right(None)
  }
}
