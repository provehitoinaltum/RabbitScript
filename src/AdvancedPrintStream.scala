package net.akouryy.common


object AdvancedPrintStream extends java.io.PrintStream(Console.out) {
  private var indent = 0
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
  override def println(a: Any) {
    super.println("  " * indent + a)
  }
}
