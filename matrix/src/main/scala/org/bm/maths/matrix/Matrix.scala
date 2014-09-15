package org.bm.maths.matrix

import java.lang.Math.min

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

/**
 * Implemented a columnlist of rows
 * <pre>
 * /         \
 * |[1, 2, 3]|
 * |[4, 5, 6]|
 * |[7, 8, 9]|
 * \         /
 * </pre>
 *
 * @author Baptiste Morin
 */


case class Matrix(rowNumber: Int, colNumber: Int, func: (Int, Int) => Int) {
  val mat: Array[Array[Int]] = {
    val m = Array.ofDim[Int](rowNumber, colNumber)
    for {
      r <- 0 until rowNumber
      c <- 0 until colNumber
    } yield m(r)(c) = func(r, c)
    m
  }


  def ==(that: Matrix): Boolean = this equals that

  override def equals(other: Any): Boolean = other match {
    case that: Matrix=> this.mat.deep == that.mat.deep
    case _ => false
  }

  def apply(r: Int, c: Int) = mat(r)(c)

  def apply(r: Int): Array[Int] = mat(r)

  def update(row: Int, col: Int, value: Int): Unit = mat(col)(row) = value

  /**
   * sum of diagonal elements
   * @return
   */
  def trace: Int = (for {
    i <- 0 until min(rowNumber, colNumber)
  } yield this(i, i)).sum


}

object Matrix {

  object Implicits {
    implicit def fromArrayOfArrayToMatrix(array: Array[Array[Int]]): Matrix = Matrix(array)
  }

  private[matrix] def constant(value: Int) = (_: Int, _: Int) => value

  def identity(rowNumber: Int, colNumber: Int) = Matrix(rowNumber, colNumber, (r, c) => if (r == c) 1 else 0)

  def apply(rowNumber: Int, colNumber: Int, value: Int): Matrix = Matrix(rowNumber, colNumber, constant(value))

  def apply(rowNumber: Int, colNumber: Int): Matrix = Matrix(rowNumber, colNumber, 0)

  def apply(array: Array[Array[Int]]): Matrix = {
    val colNumber = array.length
    if (colNumber == 0) throw new IllegalArgumentException("Array length should not be null, no cols")

    val rowNumber = array(0).length
    if (rowNumber == 0) throw new IllegalArgumentException("Array length should not be null, no rows")

    Matrix(rowNumber, colNumber, (r, c) => array(r)(c))

  }

  def apply(vector: Array[Int], numberOfRows: Int): Matrix = {
    if (vector.length % numberOfRows != 0) throw new IllegalArgumentException(s"Array length must be a multiple of $numberOfRows.")
    val numberOfCols = vector.length / numberOfRows

    Matrix(numberOfRows, numberOfCols, (r, c) => vector(r*numberOfRows + c ))
  }


}
