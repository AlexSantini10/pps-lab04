package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional
import Optional.*

/*  Exercise 5:
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others...
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A]
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

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
        case Empty() =>
          ()

@main def tryTraversable(): Unit =
  import Ex5Traversable.*

  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  val present = Just(99)
  val absent = Empty()

  consumeAll(seq)(log)
  consumeAll(present)(log)
  consumeAll(absent)(log)

  consumeAll(seq)(println)
  consumeAll(present)(println)
  consumeAll(absent)(println)