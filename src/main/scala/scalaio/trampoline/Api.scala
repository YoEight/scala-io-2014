package scalaio.trampoline

import scalaz.{ -\/, \/-, ~>, Free }
import scalaz.Id._
import scalaz.std.function._

object Api {
  type Jump[+A]      = () => A
  type Trampoline[A] = Free[Jump, A]

  def done[A](a: A): Trampoline[A] =
    Free.Return[Jump, A](a)

  def delay[A](d: => A): Trampoline[A] =
    suspend(done(d))

  def suspend[A](t: => Trampoline[A]): Trampoline[A] =
    Free.Suspend[Jump, A](() => t)

  def run[A](t: Trampoline[A]): A = {
    @annotation.tailrec
    def loop(cur: Trampoline[A]): A = cur.resume match {
      case \/-(as)   => as
      case -\/(jump) => loop(jump())
    }

    loop(t)
  }
}
