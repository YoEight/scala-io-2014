package scalaio.stream

import scalaz.{ \/, -\/, \/- }

import Api.Process

object Pipe {
  def apply[I, A, B](fa: Process[I, A], ff: Process[A, B]): Process[I, B] = {
    def go(cfa: Process[I, A], cff: Process[A, B]): Process[I, B] =
      cff.resume match {
        case -\/(s) => s match {
          case Stop()         => Api.stop
          case Emit(o, n)     => Api.emit(o).flatMap(_ => go(cfa, n))
          case Await(f, altf) => cfa.resume match {
            case -\/(fs) => fs match {
              case Await(k, alta) => Api.onAwait((i: I) => go(k(i), cff), go(alta, cff))
              case Emit(b, n)     => go(n, f(b))
              case Stop()         => go(Api.stop, altf)
            }
            case _ => go(Api.stop, altf)
          }
        }
        case _ => Api.stop
      }

    go(fa, ff)
  }

  def take[A](i: Int): Process[A, A] = i match {
    case 0 => Api.stop
    case _ => for {
      a <- Api.await
      _ <- Api.emit(a)
      r <- take(i - 1)
    } yield r
  }

  def head[A]: Process[A, A] =
    Api.compile {
      for {
        a <- Api.await
        _ <- Api.emit(a)
      } yield ()
    }

  def last[A]: Process[A, A] = {
    def loop(prev: A): Process[A, A] = for {
      a <- Api.awaitOrEmit(prev)
      r <- loop(a)
    } yield r

    Api.await.flatMap(loop(_))
  }

  def map[A, B](f: (A) => B): Process[A, B] =
    Api.repeat {
      for {
        a <- Api.await
        _ <- Api.emit(f(a))
      } yield ()
    }

  def filter[A](f: A => Boolean): Process[A, A] =
    Api.await.flatMap {
      case a if f(a) => Api.emit(a).flatMap(_ => filter(f))
      case _         => filter(f)
    }

  def find[A](f: A => Boolean): Process[A, A] =
    Pipe(filter(f), head)

  def scan[A, B](start: B)(f: (B, A) => B): Process[A, B] = {
    def loop(seed: B): Process[A, B] = for {
      _    <- Api.emit(seed)
      next <- Api.await
      r    <- loop(f(seed, next))
    } yield r

    loop(start)
  }

  def fold[A, B](start: B)(f: (B, A) => B): Process[A, B] =
    Pipe(scan(start)(f), last)
}
