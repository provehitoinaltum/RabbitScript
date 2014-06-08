package org.rabbitscript

object RabbitScript {
  def main(args: Array[String]) {
    RabbitParser parse args(0) match {
      case RabbitParser.Success(tree, _) => println(tree.toJavaScript)
      case RabbitParser.NoSuccess(err, _) => println(err)
    }
  }
}
