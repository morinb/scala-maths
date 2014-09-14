package org.bm.maths.rational

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

import scala.annotation.tailrec

/**
 * .
 * @author Baptiste Morin
 */
class Rational(val _numerator: Int, _denominator: Int) extends Ordered[Rational] {

  require(_denominator != 0)

  private val g = gcd(_numerator.abs, _denominator.abs)

  val numerator = _numerator / g
  val denominator = _denominator / g


  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)


  override def compare(that: Rational): Int = numerator * that.denominator compareTo denominator * that.numerator

  override def equals(other: scala.Any): Boolean = other match {
    case that: Rational => this.compare(that) == 0
    case _ => false
  }

  def unary_- = Rational(-numerator, denominator)

  def +(that: Rational): Rational = Rational(
    numerator * that.denominator + denominator * that.numerator,
    denominator * that.denominator
  )

  def -(that: Rational): Rational = this + -that

  def reciprocal: Rational = Rational(denominator, numerator)

  def *(that: Rational): Rational = Rational(numerator * that.numerator, denominator)

  def /(that: Rational): Rational = this * (that reciprocal)

  def /(n: Int): Rational = Rational(numerator, denominator * n)

  override def toString: String = s"$numerator/$denominator"

  private[Rational] def pow(a: Int, n: Int): Int = {

    @tailrec
    def innerPow(acc: Int, n: Int): Int = {
      if (n == 0 || n == 1) acc
      else innerPow(acc * acc, n - 1)
    }

    innerPow(a, n)
  }


  private[Rational] def atThePowerOf(n: Int): Rational = {
    if (n == 0) Rational(1, 1)
    if (n < 0 && numerator == 0) throw new ArithmeticException(s"Divide by 0 will occur when computing ($numerator/$denominator)**$n = ($denominator**${-n}/$numerator**${-n}")
    else if (n < 0) Rational(pow(denominator, -n), pow(numerator, -n))
    else Rational(pow(numerator, n), pow(denominator, n))
  }

  def **(n: Int): Rational = this atThePowerOf n
}

object Rational {
  def apply(num: Int, den: Int) = new Rational(num, den)

  def apply(real: Int) = new Rational(real, 1)

  object Implicits {
    implicit def fromInt(real: Int) = Rational(real)
  }

}