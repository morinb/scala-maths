package org.bm.maths.matrix

import java.lang.Math.min

import scala.reflect.ClassTag

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


case class Matrix[T](rowNumber: Int, colNumber: Int, func: (Int, Int) => T) {
  val mat: Array[Array[T]] = {
    val m = Array.ofDim[T](rowNumber, colNumber)
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

  def apply(r: Int): Array[T] = mat(r)

  def update(row: Int, col: Int, value: T): Unit = mat(col)(row) = value

  /**
   * sum of diagonal elements
   * @return
   */
  def trace: T = (for {
    i <- 0 until min(rowNumber, colNumber)
  } yield this(i, i)).sum


}

object Matrix {

  object Implicits {
    implicit def fromArrayOfArrayToMatrix[T: ClassTag](array: Array[Array[T]]): Matrix[T] = Matrix(array)
  }

  private[matrix] def constant[T: ClassTag](value: T) = (_: Int, _: Int) => value

  def identity(rowNumber: Int, colNumber: Int):Matrix[Int] = Matrix(rowNumber, colNumber, (r, c) => if (r == c) 1 else 0)

  def apply[T: ClassTag](rowNumber: Int, colNumber: Int, value: T): Matrix[T] = Matrix(rowNumber, colNumber, constant(value))

  def apply(rowNumber: Int, colNumber: Int): Matrix[Int] = Matrix(rowNumber, colNumber, 0)

  def apply[T: ClassTag](array: Array[Array[T]]): Matrix[T] = {
    val colNumber = array.length
    if (colNumber == 0) throw new IllegalArgumentException("Array length should not be null, no cols")

    val rowNumber = array(0).length
    if (rowNumber == 0) throw new IllegalArgumentException("Array length should not be null, no rows")

    Matrix(rowNumber, colNumber, (r, c) => array(r)(c))
  }

  def apply[T: ClassTag](vector: Array[T], numberOfRows: Int): Matrix[T] = {
    if (vector.length % numberOfRows != 0) throw new IllegalArgumentException(s"Array length must be a multiple of $numberOfRows.")
    val numberOfCols = vector.length / numberOfRows

    Matrix(numberOfRows, numberOfCols, (r, c) => vector(r*numberOfRows + c ))
  }


}
