package org.rabbitscript
import net.akouryy.common.Lib._

object RabbitScript {
  def main(args: Array[String]) {
    using(io.Source fromFile args(0)) {
      val rp = new RabbitParser
      rp parse _.getLines.mkString("\n") match {
        case rp.Success(tree, _) =>
          println(tree)
          tree.debugJavaScript eachLine (l => println(l drop 2))
        case x =>
          println(x)
      }
    }
  }
}
