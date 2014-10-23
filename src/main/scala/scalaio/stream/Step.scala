package scalaio.stream

import scalaz.Functor

trait Step[I, O, +R] {
  def map[S](f: (R) => S): Step[I, O, S]
}

case class Emit[I, O, +R](o: O, n: R) extends Step[I, O, R] {
  def map[S](f: (R) => S) = copy(n = f(n))
}

case class Await[I, O, R](k: (I) => R, alt: R) extends Step[I, O, R] {
  def map[S](f: (R) => S) = copy(k = f compose k, alt = f(alt))
}

case class Stop[I, O, +R]() extends Step[I, O, R] {
  def map[S](f: (R) => S) = Stop()
}

object Step {
  implicit def stepFuntor[I, O] =
    new Functor[({ type λ[α] = Step[I, O, α] })#λ] {
      def map[A, B](fa: Step[I, O, A])(f: A => B): Step[I, O, B] =
        fa map f
    }
}
