package org.rabbitscript
import net.akouryy.common.Lib

object RabbitScript {
  def main(args: Array[String]) {
    import Lib._
    using(io.Source.fromFile(args(0))) {
      RabbitParser parse _.getLines.mkString("\n") match {
        case RabbitParser.Success(tree, _) => println(tree.toJavaScript)
        case x => println(x)
      }
    }
  }
}
