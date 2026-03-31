package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class ComplexImpl(re: Double, im: Double)

    type Complex = ComplexImpl
    def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexImpl(real, _) => real

      def im(): Double = complex match
        case ComplexImpl(_, imag) => imag

      def sum(other: Complex): Complex = (complex, other) match
        case (ComplexImpl(re1, im1), ComplexImpl(re2, im2)) => ComplexImpl(re1 + re2, im1 + im2)

      def subtract(other: Complex): Complex = (complex, other) match
        case (ComplexImpl(re1, im1), ComplexImpl(re2, im2)) => ComplexImpl(re1 - re2, im1 - im2)

      def asString(): String = complex match
        case ComplexImpl(real, imag) if imag == 0 => s"$real"
        case ComplexImpl(real, imag) if real == 0 => s"${imag}i"
        case ComplexImpl(real, imag) => s"$real ${if imag > 0 then "+" else "-"} ${math.abs(imag)}i"

      // versione alternativa con match: s"(real, imag)"
