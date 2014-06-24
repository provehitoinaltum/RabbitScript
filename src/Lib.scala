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
  implicit val disposer       = generateCloser[{def dispose()}](_.dispose)
  implicit val closer         = generateCloser[{def close()}]  (_.close)
  implicit class AString(self: String) {
    def eachLine(f: String => Unit) = self.lines foreach f
    def mapLines[T](f: String => T) = self.lines map f mkString "\n"
    def replaceWithMap(m: Map[String, String]) = {
      def replaceWithList(s: String, l: List[(String, String)]): String =
        l match {
          case (b, a) :: ls => replaceWithList(s.replace(b, a), ls)
          case Nil => s
        }
      replaceWithList(self, m.toList)
    }
  }
}
