package scalaio.stream

import scalaz.{ \/, -\/, \/- }
import scalaz.Free

object Api {

  trait Help[I, O] {
    type C[+x] = Step[I, O, x]
  }

  type Instr[I, O, +A] = Free[Help[I, O]#C, A]
  type Process[I, O]   = Instr[I, O, Nothing]
  type Source[O]       = Process[Unit, O]

  def point[I, O, A](v: A): Instr[I, O, A] =
    Free.Return[Help[I, O]#C, A](v)

  def onEmit[I, O, A](o: O, n: Instr[I, O, A]): Instr[I, O, A] =
      Free.Suspend[Help[I, O]#C, A](Emit(o, n))

  def onAwait[I, O, A](f: I => Instr[I, O, A], alt: Instr[I, O, A]): Instr[I, O, A] =
    Free.Suspend[Help[I, O]#C, A](Await(f, alt))

  def emit[I, O](o: O): Instr[I, O, Unit] =
    onEmit(o, Free.Return[Help[I, O]#C, Unit](()))

  def await[I, O]: Instr[I, O, I] =
    onAwait(i => Free.Return[Help[I, O]#C, I](i), stop)

  def awaitOrEmit[I,O](o: O): Instr[I, O, I] =
    orElse[I, O, I](Api.await, Api.emit(o).flatMap(_ => Api.stop))

  def stop[I, O, A]: Instr[I, O, A] =
    Free.Suspend[Help[I, O]#C, A](Stop[I, O, Instr[I, O, A]])

  def orElse[I, O, A](l: Instr[I, O, A], r: Instr[I, O, A]): Instr[I, O, A] = l.resume match {
    case \/-(_) => stop
    case -\/(i) => i match {
      case Stop()      => r
      case Emit(o, n)  => onEmit(o, n)
      case Await(k, _) => onAwait(k, r)
    }
  }

  def compile[I, O, A](i: Instr[I, O, A]): Process[I, O] =
    i.flatMap(_ => stop)

  def repeat[I, O, A](p: Instr[I, O, A]): Process[I, O] =
    p.flatMap(_ => repeat(p))
}
