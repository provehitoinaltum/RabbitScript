package net.akouryy.common

object AdvancedPrintStream extends java.io.PrintStream(Console.out) {
  private var indent = 0
  private var newLine = true
  def decreaseIndent() {
    assert(indent > 0)
    indent -= 1
  }
  def increaseIndent() {
    indent += 1
  }
  def resetIndent() {
    indent = 0
  }
  override def println(s: String) {
    if(newLine)
      super.print("  " * indent)
    newLine = false
    super.println(s)
    newLine = true
  }
  override def println(a: Any) {
    if(newLine)
      super.print("  " * indent)
    newLine = false
    super.println(a)
    newLine = true
  }
  override def print(s: String) {
    if(newLine)
      super.print("  " * indent)
    newLine = false
    super.print(s)
    newLine = false
  }
  override def print(a: Any) {
    if(newLine)
      super.print("  " * indent)
    newLine = false
    super.print(a)
    newLine = false
  }
}
