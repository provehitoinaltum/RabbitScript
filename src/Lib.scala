package net.akouryy.common

object Lib {
  import language.reflectiveCalls
  def using[A, B](resource: A)(func: A => B)(implicit closer: Closer[A]) =
    try {
      func(resource)
    } finally {
      closer(resource)
    }
  trait Closer[-A] {
    def apply(resource: A)
  }
  def generateCloser[A](f: A => Unit) = {
    new Closer[A] {
      def apply(resource: A) = f(resource) 
    }
  }
  implicit val closer   = generateCloser[{def close()}]  (_.close)
  implicit val disposer = generateCloser[{def dispose()}](_.dispose)

  implicit class AString(self: String) {
    def eachLine(f: String => Unit) = self.lines foreach f
    def mapLines(f: String => String) = self.lines map f mkString "\n"
    def replaceWithList(l: List[(String, String)]) = {
      def replaceWithList2(s: String, l: List[(String, String)]): String =
        l match {
          case (b, a) :: ls => replaceWithList2(s.replace(b, a), ls)
          case Nil => s
        }
        replaceWithList2(self, l)
    }
    def replaceWithMap(m: Map[String, String]) = replaceWithList(m.toList)
  }
}
