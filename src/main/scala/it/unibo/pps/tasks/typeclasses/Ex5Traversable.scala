package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.extensionmethods.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.extensionmethods.Optionals.Optional
import Optional.*

object Ex5Traversable:

  trait Traversable[T[_]]:
    def foreach[A](container: T[A])(consumer: A => Unit): Unit

  def log[A](a: A): Unit =
    println("The next element is: " + a)

  def consumeAll[T[_]: Traversable, A](container: T[A])(consumer: A => Unit): Unit =
    summon[Traversable[T]].foreach(container)(consumer)

  given Traversable[Sequence] with
    def foreach[A](container: Sequence[A])(consumer: A => Unit): Unit =
      container match
        case Cons(h, t) =>
          consumer(h)
          foreach(t)(consumer)
        case Nil() =>
          ()

  given Traversable[Optional] with
    def foreach[A](container: Optional[A])(consumer: A => Unit): Unit =
      container match
        case Just(value) =>
          consumer(value)
        case None() =>
          ()

@main def tryTraversable(): Unit =
  import Ex5Traversable.*

  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  val present = Just(99)
  val absent = None()

  consumeAll(seq)(log)
  consumeAll(present)(log)
  consumeAll(absent)(log)

  consumeAll(seq)(println)
  consumeAll(present)(println)
  consumeAll(absent)(println)