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

import java.lang.Math._

/**
 * .
 * @author Baptiste Morin
 */
trait Maths {
  def hypot(a: Double, b: Double): Double =
    if (abs(a) > abs(b)) {
      val r = b / a
      abs(a) * sqrt(1 + r * r)
    } else if (b != 0) {
      val r = a / b
      abs(b) * sqrt(1 + r * r)
    } else 0
}
