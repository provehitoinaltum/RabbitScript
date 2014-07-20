package net.akouryy.common

object Lib {
  implicit class AFunc1[A, B](self: A ⇒ B) {
    def <*>[C](f: B ⇒ C) = { a: A ⇒ f(self(a)) }
    def </>[C](f: C ⇒ A) = { c: C ⇒ self(f(c)) }
    def <#>(a: A) = self(a)
  }
  implicit class ASource[S <: io.Source](self: S) {
    def ->->[A](f: S ⇒ A) {
      try {
        f(self)
      } finally {
        self.close
      }
    }
    def →→ = ->-> _
    def beforeClose = ->-> _
  }
  implicit class AString(self: String) {
    def eachLine(f: String ⇒ Unit) = self.lines foreach f
    def mapLines(f: String ⇒ String) = self.lines map f mkString "\n"
    def replaceWithTuples(a: (String, String)*) = replaceWithList(a.toList)
    def replaceWithList(l: List[(String, String)]) = (self /: l) {(s, t) ⇒ s.replace(t._1, t._2)}
    def replaceWithMap(m: Map[String, String]) = replaceWithList(m.toList)
  }
}
