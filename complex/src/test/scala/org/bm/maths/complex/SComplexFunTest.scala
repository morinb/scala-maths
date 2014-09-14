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

import org.bm.maths.complex.Complex.Implicits._
import org.bm.maths.complex.Complex.i
import org.scalatest.FunSuite

/**
 * .
 * @author Baptiste Morin
 */
class SComplexFunTest extends FunSuite {
  test("org.bm.maths.complex.SComplex(1)") {
    val c: Complex = Complex(1)
    assert(c !== null)
  }

  test("i") {
    assert(i.toString === Complex(0, 1).toString)
  }

  test("toString") {
    assert((7 + 5 * i).toString === "7.0+5.0*i")
    assert(i.toString === "i")
  }

  test("addition") {
    val c1 = Complex(7, 5)
    val c2 = Complex(8, -3)
    println(s"c1 = $c1")
    println(s"c2 = $c2")

    val expected = Complex(15, 2)

    assert(expected == c1 + c2)
    assert(expected === c1 + c2)

  }

  test("modulus") {
    val c = Complex(3, 4)
    assert(!c === 5)
  }

  test("i^2") {
    val r = i * i
    val e = -1.0
    assert(i * i === Complex(-1.0))
  }

  test("operators") {
    val c = 7 + 5 * i
    val expected = Complex(7, 5)

    assert(expected === c)
  }

  test("reciprocal") {
    val c = 1 + 2 * i
    val expected = 0.2 - 0.4 * i

    assert(expected === c.reciprocal)
  }

}
