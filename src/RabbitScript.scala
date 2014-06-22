package org.rabbitscript
import net.akouryy.common.Lib

object RabbitScript {
  def main(args: Array[String]) {
    import Lib._
    using(io.Source.fromFile(args(0))) {
      val rp = new RabbitParser
      rp parse _.getLines.mkString("\n") match {
        case rp.Success(tree, _) =>
          println(tree)
          for(x <- tree.debugJavaScript.lines)
            println(x drop 2)
        case x =>
          println(x)
      }
    }
  }
}
