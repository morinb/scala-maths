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

package org.bm.maths.rational

import org.scalatest.FunSuite

/**
 * .
 * @author Baptiste Morin
 */
class RationalFunTest extends FunSuite {

  test("powerOf") {
    val r : Rational = Rational(2, 3)

    val expected = Rational(4, 9)

    assert(expected === r ** 2)
  }

  test("reduction") {
    assert(Rational(2/4).toString === Rational(1/2).toString)
  }
}
