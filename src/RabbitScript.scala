package org.rabbitscript
import net.akouryy.common._
import Lib._

object RabbitScript {
  val Version = "α45"
  def main(args: Array[String]) {
    val clp = new CommandLineParser[Unit]
    var color = false
    var warningLevel = 1
    var logParser = false

    clp(false, "color") = CommandLineOption (
      arity = 0,
      handler = {
        case Nil ⇒
          color = true
          println(ResetConsole)
          Right(())
      }
    )
    clp(false, 'w', "warning") = CommandLineOption (
      arity = 1,
      handler = {
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
    clp(false, "log:parser") = CommandLineOption (
      arity = 0,
      handler = {
        case Nil ⇒
          logParser = true
          Right(())
      }
    )
    clp(true, 'v', "version") = CommandLineOption (
      arity = 0,
      handler = { case Nil ⇒ println(s"RabbitScript ver. $Version\n"); Right(()) }
    )
    clp(true, 'h', "help") = CommandLineOption (
      arity = 0,
      handler = {
        case Nil ⇒
          println("""Is the order help?
                    |Usage: rabbit {<option>} [<filename>]
                    |     | rabbit <command>
                    |
                    |<option>:
                    |  -w --warning [0-3]  set warning level
                    |
                    |     --color          enable colorful output
                    |     --log:parser     display parser log
                    |
                    |<command>:
                    |  -v --version        show version number
                    |  -h --help           show this help message
                    |""".stripMargin)
          Right(())
      }
    )
    clp.main = { args ⇒
      try {
        (io.Source fromFile args(0)) →→ { sc ⇒
          val rp = new RabbitParser(logParser, color)
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
    args.toList match {
      case a @ _ :: _ ⇒
        clp parse args.toList match {
          case Left(s) ⇒
            Console.err println s
            Console.err println "try -h for more information"
          case Right(_) ⇒
        }
      case Nil ⇒
        println("try -h for the information")
    }
  }
}

