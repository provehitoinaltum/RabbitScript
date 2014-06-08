package org.rabbitscript

object RabbitScript {
  def main(args: Array[String]) {
    println(new RabbitParser() parse args(0))
  }
}
