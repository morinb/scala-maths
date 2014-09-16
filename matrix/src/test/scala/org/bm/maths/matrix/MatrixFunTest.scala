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
    println("identity")
    println("--------")
    val eye3 = identity(3, 3)

    assert(eye3(0, 0) === 1)
    assert(eye3(1, 1) === 1)
    assert(eye3(2, 2) === 1)

  }

  test("trace") {
    println("trace")
    println("-----")
    val eye3 = identity(3, 3)

    assert(eye3.trace === 3)
  }

  test("copy") {
    println("copy")
    println("----")

    val arr: Array[Array[Int]] = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))

    val m = Matrix(arr)

    assert(m(0)(2) === 3)
    assert(m(1)(1) === 5)
    assert(m(2)(2) === 9)
  }

  test("copy from vector") {
    println("copy from vector")
    println("----------------")

    val expected: Matrix = Matrix(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))
    val vector = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)

    val m = Matrix(vector, 3)

    assert(expected === m)

  }

  test("toString") {
    println("tostring")
    println("--------")

    val m: Matrix = Matrix(4, 4, (r, c) => if ((r + c) % 2 == 0) 1.0 else 0.0)

    println(m)
  }

  test("submatrix") {
    println("submatrix")
    println("---------")

    val expected: Matrix = Matrix(Array(Array(1, 2), Array(4, 5)))
    val computed: Matrix = Matrix(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))((0, 1), (0, 1))

    println(expected)

    assert(expected === computed)
  }

  test("set submatrix") {
    println("set submatrix")
    println("-------------")

    val m = Matrix(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))
    val sub: Matrix = Matrix(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))((1, 2), (1, 2))
    m((0, 1), (0, 1)) = sub
    val computed: Matrix = m

    val expected = Matrix(Array(Array(5, 6, 3), Array(8, 9, 6), Array(7, 8, 9)))

    println(computed)
    assert(expected === computed)
  }

  test("set submatrix 2") {
    println("set submatrix 2")
    println("---------------")

    val m = Matrix(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))
    val expected = Matrix(Array(Array(4, 6), Array(7, 9)))
    val sub = m(Array(1, 2), Array(0, 2))


    println(sub)
    assert(expected === sub)
  }

  test("transpose") {
    println("transpose")
    println("---------")

    val m = Matrix(Array(Array(1, 2, 3), Array(4, 5, 6)))

    assert(m === m.t.t)
  }



  test("filter") {
    println("filter")
    println("------")

    val m = Matrix(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))
    def f(d: Double): Boolean = d % 2 == 0

    val computed = m.filter(f)

    println(computed)
  }

  test("find") {
    println("find")
    println("----")

    val m = Matrix(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))

    val d = m.find(x => x > 4)

    println(m)
    println(d)
  }

  test("augment :+") {
    println("augment :+")
    println("----------")

    val m = Matrix(Array(
      Array(2, 1, -1),
      Array(-3, -1, 2),
      Array(-2, 1, 2)
    ))

    val vector = Matrix(Array(
      Array(8),
      Array(1),
      Array(5)
    ))
    val expected = Matrix(Array(
      Array(2, 1, -1, 8),
      Array(-3, -1, 2, 1),
      Array(-2, 1, 2, 5)
    ))

    val result = m :+ vector

    println(result)
    assert(expected === result)

  }

  test("prepend +:") {
    println("prepend +:")
    println("----------")

    val m = Matrix(Array(
      Array(2, 1, -1),
      Array(-3, -1, 2),
      Array(-2, 1, 2)
    ))

    val vector = Matrix(Array(
      Array(8),
      Array(1),
      Array(5)
    ))
    val expected = Matrix(Array(
      Array(8, 2, 1, -1),
      Array(1, -3, -1, 2),
      Array(5, -2, 1, 2)
    ))

    val result = vector +: m

    println(result)
    assert(expected === result)

  }

  test("determinant") {
    println("determinant")
    println("-----------")

    val expected: Double = 18
    val m = Matrix(Array(
      Array(-2, 2, -3),
      Array(-1, 1, 3),
      Array(2, 0, -1)
    ))

    assert(expected === m.det)

  }

  test("LU") {
    val m = Matrix(Array(1, 4, 7, 2, 5, 8, 3, 6, 9), 3)

    val luDecomposition = LUDecomposition(m)

    val L = luDecomposition.L
    val U = luDecomposition.U
    val LU = L * U
    println(m)
    println(s"L: ${luDecomposition.L}")
    println(s"U: ${luDecomposition.U}")
    println(s"Det: ${luDecomposition.det}")

    println("luDecomposition=" + (luDecomposition.L * luDecomposition.U))

    assert(m(luDecomposition.pivot, 0, 2) === LU)


  }

  test("normF") {
    val m = Matrix(Array(1, -4, 7, 2, 5, 8, 3, 6, 9), 3)
    println(m)
    println(m.normF)
    println(m.norm1)
    //println(m.norm2)
    println(m.normInf)
  }


}
