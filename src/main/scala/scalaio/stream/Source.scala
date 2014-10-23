package scalaio.stream

import scalaz.Free

import Api.Process

object Source {
  type Source[A]    = Process[Unit, A]

  trait Damn[O] {
    type M[x] = Api.Instr[Unit, O, x]
  }

  def source[A](list: List[A]): Source[A] = list match {
    case a :: as => Api.emit(a).flatMap(_ => source(as))
    case _       => Api.stop
  }

  def enumerate(from: Int, to: Int): Source[Int] = {
    def loop(cur: Int): Source[Int] =
      if (cur > to)
        Api.stop
      else Api.emit(cur).flatMap(_ => loop(cur + 1))

    loop(from)
  }

  def repeat[A](v: A): Source[A] =
    Api.repeat(Api.emit(v))
}
