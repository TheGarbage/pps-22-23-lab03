package Task

import u03.Streams.Stream

import scala.annotation.tailrec

object Task extends App:

  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:

    // Task 1a
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(_, t) if n != 0 => drop(t, n - 1)
      case _ => l

    // Task 1b
    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case _ => right

    // Task 1c
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()

    // Task 1d
    def map[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(A => Cons(mapper(A), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] =
      flatMap(l1)(h => h match
        case n if pred(n) => Cons(n, Nil())
        case _ => Nil()
      )

    // Task 2
    import Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None()
      case Cons(h, _) =>
        @tailrec
        def max(l: List[Int], m: Int): Int = l match
          case Cons(h, t) if h > m => max(t, h)
          case Cons(_, t) => max(t, m)
          case _ => m
        Some(max(l, h))

    // Task 3
    import Person.*
    def coursesExtraction(l: List[Person]): List[String] =
      flatMap(l)(p => p match
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil())

    // Task 4
    @tailrec
    def foldLeft[A, B](l: List[A])(a: B)(f: (B, A) => B) : B = l match
      case Cons(h, t) => foldLeft(t)(f(a, h))(f)
      case _ => a

    def foldRight[A, B](l: List[A])(a: B)(f: (A, B) => B): B = l match
      case Cons(h, t) => f(h, foldRight(t)(a)(f))
      case _ => a
  end List

  // Task 5-6-7
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:
    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(_, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 5
    @tailrec
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(_, t), n) if n > 0 => drop(t())(n - 1)
      case _ => stream

    // Task 6
    def constant[A](const: => A): Stream[A] =
      cons(const, constant(const))

    // Task 7
    def fibs: Stream[Int] =
      def fibStream(a: Int): Stream[Int] =
        def fibsInt(n: Int): Int = n match
          case n if n == 0 || n == 1 => n
          case _ => fibsInt(n - 1) + fibsInt(n - 2)
        Cons(() => fibsInt(a), () => fibStream(a + 1))
      fibStream(0)

  end Stream