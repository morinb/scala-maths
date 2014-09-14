package org.bm.maths.complex

/*
 * Scala math library.
 * Copyright (C) 2014  Baptiste MORIN
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

import java.lang.Math.{pow, sqrt}

/**
 * .
 * @author Baptiste Morin
 */
case class Complex(_real: Double, _imag: Double) extends Ordered[Complex] {
  lazy val real = _real

  lazy val imag = _imag

  private val modulus = sqrt(pow(real, 2) + pow(imag, 2))

  def unary_! = modulus

  def +(that: Complex): Complex = Complex(real + that.real, imag + that.imag)

  def +(that: Int): Complex = this + Complex(that)

  def unary_- : Complex = Complex(-real, -imag)

  def unary_~ : Complex = Complex(real, -imag)

  def -(that: Complex): Complex = this + -that

  def *(that: Complex): Complex = Complex(real * that.real - imag * that.imag, real * that.imag + imag * that.real)

  def /(that: Complex): Complex = {
    val a = real
    val b = imag
    val c = that.real
    val d = that.imag

    Complex(
      (a * c + b * d) / (c * c + d * d), (b * c - a * d) / (c * c + d * d)
    )
  }

  def isZero = real == 0.0 && imag == 0.0

  def reciprocal = if (isZero) throw new ArithmeticException(s"$this is zero. Can not divide by zero") else Complex(1) / this


  override def equals(that: scala.Any): Boolean = that match {
    case t: Complex => this.compare(t) == 0
    case _ => false
  }

  override def toString = this match {
    case Complex.i => "i"
    case Complex(re, 0) => re.toString
    case Complex(0, im) => im.toString + "*i"
    case _ => asString
  }


  private def asString: String = {
    val sb = new StringBuilder
    if (real != 0) {
      sb ++= s"$real"
    }
    if (imag < 0)
      sb ++= s"$imag*i"
    else if (imag > 0) {
      if (real != 0) {
        sb ++= "+"
      }
      sb ++= s"$imag*i"
    }

    sb.toString()
  }

  override def compare(that: Complex): Int = !this compareTo !that
}

object Complex {
  val i = Complex(0, 1)

  def apply(real: Double): Complex = new Complex(real, 0)

//  def apply(real: Double, imag: Double): Complex = new Complex(real, imag)

  object Implicits {
    implicit def fromDoubleToComplex(real: Double): Complex = Complex(real)

  }

}
