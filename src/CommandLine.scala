package net.akouryy.common

case class CommandLineOption(arity: Int, handler: PartialFunction[List[String], Either[String, Unit]])

class CommandLineParser[A] {
  private val options = collection.mutable.Map[String, (CommandLineOption, Boolean)]()
  private var _main: List[String] ⇒ Either[String, A] = _
  def update(exit: Boolean, shortName: Char, longName: String, clo: CommandLineOption) {
    options("-" + shortName) = (clo, exit)
    options("--" + longName) = (clo, exit)
  }
  def update(exit: Boolean, longName: String, clo: CommandLineOption) {
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
          case Some((CommandLineOption(arity, handler), false)) ⇒
            val (take, drop) = tail splitAt arity
            if(take.length == arity)
              handler(take) match {
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
