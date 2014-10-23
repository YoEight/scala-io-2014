package scalaio.trampoline

import Api.Trampoline

object MutualRecursion {
  def map[A, B](list: List[A])(f: (A) => B): List[B] = {
    def loop(cur: List[A]): Trampoline[List[B]] = cur match {
      case a :: as => Api.suspend(loop(as)).map(f(a) :: _)
      case _       => Api.done(Nil)
    }

    Api.run(loop(list))
  }

  def foldRight[A, B](seed: B, list: List[A])(f: (A, B) => B): B = {
    def loop(cur: List[A]): Trampoline[B] = cur match {
      case a :: as => Api.suspend(loop(as)).map(f(a, _))
      case _       => Api.done(seed)
    }

    Api.run(loop(list))
  }

  def quicksort(list: List[Int]): List[Int] = {
    def sort(cur: List[Int]): Trampoline[List[Int]] = cur match {
      case p :: xs =>
        val lesser  = xs.filter(_ < p)
        val greater = xs.filter(_ >= p)

        for {
          sh <- Api.suspend(sort(lesser))
          st <- Api.suspend(sort(greater))
        } yield (sh :+ p) ::: st
      case _ => Api.done(Nil)
    }

    Api.run(sort(list))
  }
}

object StackOverflow { // Usually SOE occurs from 10000 elements
  def map[A, B](list: List[A])(f: (A) => B): List[B] = list match {
    case a :: as => f(a) :: map(as)(f)
    case _       => Nil
  }

  def foldRight[A, B](seed: B, list: List[A])(f: (A, B) => B): B = list match {
    case a :: as => f(a, foldRight(seed, as)(f))
    case _       => seed
  }

  def quicksort(list: List[Int]): List[Int] = list match {
    case p :: xs =>
      val lesser  = xs.filter(_ < p)
      val greater = xs.filter(_ >= p)

      (quicksort(lesser) :+ p) ::: quicksort(greater)
    case _ => Nil
  }
}
