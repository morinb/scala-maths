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

package org.bm.maths.matrix

import org.bm.maths.matrix.Matrix._
import org.bm.maths.matrix.Matrix.Implicits._
import org.scalatest.FunSuite

/**
 * .
 * @author Baptiste Morin
 */
class MatrixFunTest extends FunSuite {

  test("identity") {
    val eye3 = identity(3, 3)

    assert(eye3(0, 0) === 1)
    assert(eye3(1, 1) === 1)
    assert(eye3(2, 2) === 1)

  }

  test("trace") {
    val eye3 = identity(3, 3)

    assert(eye3.trace === 3)
  }

  test("copy") {
    val arr = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))

    val m = Matrix(arr)

    assert(m(0)(2) === 3)
    assert(m(1)(1) === 5)
    assert(m(2)(2) === 9)
  }

  test("copy from vector") {
    val expected: Matrix[Int] = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
    val vector = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)

    val m = Matrix(vector, 3)

    assert(expected === m)

  }
}
