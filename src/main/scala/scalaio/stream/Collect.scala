package scalaio.stream

import scalaz.{ \/, -\/, \/- }

import Api.Process

object Collect {
  def apply[I, O](p: Process[I, O]): List[O] = {
    def loop(bs: List[O], cur: Process[I, O]): List[O] =
      cur.resume match {
        case \/-(x) => bs
        case -\/(i) => i match {
          case Emit(b, n)   => loop(bs :+ b, n)
          case Await(_,alt) => loop(bs, alt)
          case _            => bs
        }
      }

    loop(Nil, p)
  }
}
