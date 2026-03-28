package it.unibo.pps.tasks.adts

import it.unibo.pps.u03.extensionmethods.Sequences.Sequence, Sequence.*
import it.unibo.pps.u03.extensionmethods.Optionals.Optional, Optional.*

/*  Exercise 3: 
 *  Implement a Stack ADT
 *  Suggestion: 
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional is stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    case class StackImpl[A](elements: Sequence[A])

    type Stack[A] = StackImpl[A]

    def empty[A]: Stack[A] =
      StackImpl(Nil())

    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] =
        StackImpl(Cons(a, stack.elements))

      def pop(): Optional[(A, Stack[A])] =
        stack.elements match
          case Nil() =>
            None()
          case Cons(head, tail) =>
            Just((head, StackImpl(tail)))

      def asSequence(): Sequence[A] =
        stack.elements