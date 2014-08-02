package org.rabbitscript
import net.akouryy.common._
import lib.{ResetConsole ⇒ _, _}

object RabbitScript {
  object Options {
    var color = false
    def ResetConsole = if(color) lib.ResetConsole else ""
    def Black      = if(color) Console.BLACK      else ""
    def BlackB     = if(color) Console.BLACK_B    else ""
    def Blink      = if(color) Console.BLINK      else ""
    def Blue       = if(color) Console.BLUE       else ""
    def BlueB      = if(color) Console.BLUE_B     else ""
    def Bold       = if(color) Console.BOLD       else ""
    def Cyan       = if(color) Console.CYAN       else ""
    def CyanB      = if(color) Console.CYAN_B     else ""
    def Green      = if(color) Console.GREEN      else ""
    def GreenB     = if(color) Console.GREEN_B    else ""
    def Invisible  = if(color) Console.INVISIBLE  else ""
    def Magenta    = if(color) Console.MAGENTA    else ""
    def MagentaB   = if(color) Console.MAGENTA_B  else ""
    def Red        = if(color) Console.RED        else ""
    def RedB       = if(color) Console.RED_B      else ""
    def Reset      = if(color) Console.RESET      else ""
    def Reversed   = if(color) Console.REVERSED   else ""
    def Underlined = if(color) Console.UNDERLINED else ""
    def White      = if(color) Console.WHITE      else ""
    def WhiteB     = if(color) Console.WHITE_B    else ""
    def Yellow     = if(color) Console.YELLOW     else ""
    def YellowB    = if(color) Console.YELLOW_B   else ""
    var warningLevel = 1
    var logParser = false
    var logTree = false
  }

  val Version = "α46"
  def main(args: Array[String]) {
    val clp = new CommandLineParser[Unit]

/*
    clp(false, "color") = CommandLineOption (
      arity = 0,
      handler = {
        case Nil ⇒
          Options.color = true
          print(Options.ResetConsole)
          Right(())
      }
    )
*/
    clp(false, 'w', "warning") = CommandLineOption (
      arity = 1,
      handler = {
        case w :: Nil ⇒
          try {
            val warning = w.toInt
            if(0 <= warning) {
              if(warning <= 3) {
                Options.warningLevel = warning
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
    clp(false, "log") = CommandLineOption (
      arity = 1,
      handler = {
        case "parser" :: Nil ⇒
          Options.logParser = true
          Right(())
        case "tree" :: Nil ⇒
          Options.logTree = true
          Right(())
        case l :: Nil ⇒ Left("can't log " + l)
      }
    )
    clp(true, 'v', "version") = CommandLineOption (
      arity = 0,
      handler = {
        case Nil ⇒
          println("RabbitScript ver. " + Version)
          Right(())
      }
    )
    clp(true, 'h', "help") = CommandLineOption (
      arity = 0,
      handler = {
        case Nil ⇒
          println("""Is the order help?
                    |
                    |Usage: rabbit {<option>} [<filename>]
                    |     | rabbit <command>
                    |
                    |<option>:
                    |  -w --warning      set warning level
                    |
                    |     --log          display log
                    |
                    |<command>:
                    |  -v --version      show version number
                    |  -h --help         show this help message
                    |  -i --information  show some informations of commands
                    |
                    |Try -i <command> for more details.
                    |""".stripMargin)
//                  |     --color        enable colorful output
          Right(())
      }
    )
    clp(true, 'i', "information") = CommandLineOption (
      arity = 1,
      handler = {
        case ("-w" | "--warning") :: Nil ⇒
          println("""-w --warning
                    |Arguments:
                    |  1. warning level (0-3)
                    |
                    |set warning level
                    |""".stripMargin)
          Right(())
/*
        case "--color" :: Nil ⇒
          println(s"--color : ${Console.BOLD}${Console.RED}enable ${Console.GREEN}colorful ${Console.CYAN}output${Lib.ResetConsole}")
          Right(())
*/
        case "--log" :: Nil ⇒
          println("""--log
                    |Arguments:
                    |  1. the name of what to show log (parser | tree)
                    |
                    |display log
                    |""".stripMargin)
          Right(())
        case ("-v" | "--version") :: Nil ⇒
          println("""-v --version
                    |Arguments: None
                    |
                    |show the version information of RabbitScript
                    |""".stripMargin)
          Right(())
        case ("-h" | "--help") :: Nil ⇒
          println("""-h --help
                    |Arguments: None
                    |
                    |show the help message
                    |""".stripMargin)
          Right(())
        case ("-i" | "--information") :: Nil ⇒
          println("""-i --information
                    |Arguments:
                    |  1. the name of the command to show
                    |
                    |show the information of given command
                    |""".stripMargin)
          Right(())
        case c :: Nil ⇒ Left("no information of " + c + "found")
      }
    )
    clp.main = { args ⇒
      try {
        (io.Source fromFile args(0)) →→ { sc ⇒
          val rp = new RabbitParser
          rp parse sc.getLines.mkString("\n") match {
            case rp.Success(tree, _) ⇒
              if(Options.logTree)
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
            Console.err println "Try -h for more information."
          case Right(_) ⇒
        }
      case Nil ⇒
        Console.err println "Try -h for the information."
    }
  }
}

