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
}
