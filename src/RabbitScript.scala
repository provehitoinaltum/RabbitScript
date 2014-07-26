package org.rabbitscript
import net.akouryy.common.Lib._

import collection._

object RabbitScript {
  val Version = "α39"
  def main(args: Array[String]) {
    val cla = new CommandLineArgument[Unit]
    var warningLevel = 1
    cla(false, 'w', "warning") = CommandLineOption (
      arity = 1,
      f = {
        case w :: Nil ⇒
          try {
            val warning = w.toInt
            if(0 <= warning) {
              if(warning <= 3) {
                warningLevel = warning
                Right(())
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
    cla(true, 'v', "version") = CommandLineOption (
      arity = 0,
      f = { case Nil ⇒ println(s"RabbitScript ver. $Version\n"); Right(()) }
    )
    cla(true, 'h', "help") = CommandLineOption (
      arity = 0,
      f = {
        case Nil ⇒
          println("""Is the order help?
                    |Usage: rabbit {<option>} [<filename>]
                    |     | rabbit <command>
                    |
                    |<option>:
                    |  -w --warning [0-3]  set warning level
                    |
                    |<command>:
                    |  -v --version        show version number
                    |  -h --help           show this help message
                    |""".stripMargin)
          Right(())
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
        Right(())
      } catch {
        case _: java.io.FileNotFoundException ⇒
          Left(s"file ${args(0)} not found")
      }
    }
    cla parse args.toList match {
      case Left(s) ⇒
        Console.err println s
        Console.err println "try -h for more information"
      case Right(_) ⇒
    }
  }
}

case class CommandLineOption(arity: Int, f: PartialFunction[List[String], Either[String, Unit]])

class CommandLineArgument[A] {
  private val options = mutable.Map[String, (CommandLineOption, Boolean)]()
  private var _main: List[String] ⇒ Either[String, A] = _
  def update(exit: Boolean, shortName: Char, longName: String, clo: CommandLineOption) {
    options("-" + shortName) = (clo, exit)
    options("--" + longName) = (clo, exit)
  }
  def main{}
  def main_=(main: List[String] ⇒ Either[String, A]) {
    _main = main
  }
  def parse(args: List[String]): Either[String, Option[A]] = args match {
    case init :: tail ⇒
      if(init(0) == '-')
        options.get(init) match {
          case Some((CommandLineOption(arity, f), true)) ⇒
            if(tail.length == arity)
              f(tail) match {
                case Left(s) ⇒ Left(s"something wrong in $init: $s")
                case Right(()) ⇒ Right(None)
              }
            else
              Left(s"bad argument length: ${tail.length} for $arity")
          case Some((CommandLineOption(arity, f), false)) ⇒
            val (take, drop) = tail splitAt arity
            if(take.length == arity)
              f(take) match {
                case Left(s) ⇒ Left(s"something wrong in $init: $s")
                case Right(()) ⇒ parse(drop)
              }
            else
              Left(s"bad argument length: ${take.length} for $arity")
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
