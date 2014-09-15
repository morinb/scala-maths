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

import java.lang.Math.min
import java.text.NumberFormat

import scala.util.Random

/**
 *
 * @author Baptiste Morin
 */


case class Matrix(rowNumber: Int, colNumber: Int, func: (Int, Int) => Double) {
  require(rowNumber > 0, "Row number must be positive")
  require(colNumber > 0, "Col number must be positive")

  val mat: Array[Array[Double]] = {
    val m: Array[Array[Double]] = Array.ofDim[Double](rowNumber, colNumber)
    for {
      r <- 0 until rowNumber
      c <- 0 until colNumber
    } yield m(r)(c) = func(r, c)
    m
  }


  def ==(that: Matrix): Boolean = this equals that

  override def equals(other: Any): Boolean = other match {
    case that: Matrix => this.deep == that.deep
    case _ => false
  }

  def deep = this.mat.deep

  def apply(r: Int, c: Int) = mat(r)(c)

  def apply(r: Int): Array[Double] = mat(r)

  def update(row: Int, col: Int, value: Double): Unit = mat(col)(row) = value

  def update(rows: (Int, Int), cols: (Int, Int), m: Matrix): Unit = setSubMatrix(rows, cols, m)

  def foreach(f: Double => Double): Unit = map(f)

  def filter(f: Double => Boolean)(implicit _zero: Double): Matrix = map(x => if (f(x)) x else _zero)

  def find(f: Double => Boolean): Option[Double] = {
    var these: Double = 0
    for (r <- 0 until rowNumber) {
      for (c <- 0 until colNumber) {
        val t = this(r)(c)
        if (f(t)) return Some(t)
      }
    }
    None
  }

  def map(f: Double => Double): Matrix = Matrix(this.mat.flatten.map(f), rowNumber)


  def >+(d: Double): Matrix = Matrix(this).map(_ + d)

  def >-(d: Double): Matrix = Matrix(this).map(_ - d)

  def >*(d: Double): Matrix = Matrix(this).map(_ * d)

  def >/(d: Double): Matrix = Matrix(this).map(_ / d)

  def *(that: Matrix): Matrix = ???

  def /(that: Matrix): Matrix = ???

  def `\`(that: Matrix): Matrix = ???

  def +(that: Matrix): Matrix = ???

  def -(that: Matrix): Matrix = ???

  /**
   * transpose
   * @return
   */
  def t: Matrix = Matrix(colNumber, rowNumber, (r, c) => this(c)(r))

  private[matrix] def setSubMatrix(rows: (Int, Int), cols: (Int, Int), m: Matrix): Unit = {
    val startRow = rows._1
    val endRow = rows._2
    val startCol = cols._1
    val endCol = cols._2

    for {
      r <- startRow to endRow
      c <- startCol to endCol
    } yield this(r)(c) = m(r - startRow)(c - startCol)

  }

  def apply(upperLeftCorner: (Int, Int), lowerRightCorner: (Int, Int)): Matrix = submatrix(upperLeftCorner, lowerRightCorner)

  private[matrix] def submatrix(rows: (Int, Int), cols: (Int, Int)): Matrix = {
    val startRow = rows._1
    val endRow = rows._2
    val startCol = cols._1
    val endCol = cols._2

    val rowN = endRow - startRow + 1
    val colN = endCol - startCol + 1

    val m = Array.ofDim[Double](rowN, colN)

    for {
      r <- startRow to endRow
      c <- startCol to endCol
    } yield m(r - startRow)(c - startCol) = mat(r)(c)

    Matrix(m)
  }

  /**
   * sum of diagonal elements
   * @return
   */
  def trace: Double = (for {
    i <- 0 until min(rowNumber, colNumber)
  } yield this(i, i)).sum

  import org.bm.maths.matrix.Matrix.Implicits.numberFormat

  override def toString = asString()

  def asString()(implicit nf: NumberFormat) = {
    val sb: StringBuilder = new StringBuilder()
    mat.foreach(doubleArray =>
      sb append doubleArray.map(d => nf.format(d)).mkString("[", "  ", "]\n")
    )
    sb.toString()
  }


}

object Matrix {

  object Implicits {
    implicit val zero: Double = 0

    implicit val numberFormat: NumberFormat = NumberFormat.getNumberInstance

    implicit def fromArrayOfArrayToMatrix(array: Array[Array[Double]]): Matrix = Matrix(array)

    implicit def intArray2DoubleArray(array: Array[Int]): Array[Double] = array.map(_.toDouble)

    implicit def intArrayArray2DoubleArrayArray(array: Array[Array[Int]]): Array[Array[Double]] = array.map(_.map(_.toDouble))
  }

  private[matrix] def constant(value: Double) = (_: Int, _: Int) => value

  def identity(rowNumber: Int, colNumber: Int): Matrix = Matrix(rowNumber, colNumber, (r, c) => if (r == c) 1 else 0)

  def random(rowNumber: Int, colNumber: Int): Matrix = Matrix(rowNumber, colNumber, Random.nextDouble())

  def apply(rowNumber: Int, colNumber: Int, value: Double): Matrix = Matrix(rowNumber, colNumber, constant(value))

  def apply(rowNumber: Int, colNumber: Int): Matrix = Matrix(rowNumber, colNumber, 0)

  def apply(array: Array[Array[Double]]): Matrix = {
    val rowNumber = array.length
    if (rowNumber == 0) throw new IllegalArgumentException("Array length should not be null, no cols")

    val colNumber = array(0).length
    if (colNumber == 0) throw new IllegalArgumentException("Array length should not be null, no rows")

    Matrix(rowNumber, colNumber, (r, c) => array(r)(c))
  }

  def apply(vector: Array[Double], numberOfRows: Int): Matrix = {
    if (vector.length % numberOfRows != 0) throw new IllegalArgumentException(s"Array length must be a multiple of $numberOfRows.")
    val numberOfCols = vector.length / numberOfRows

    Matrix(numberOfRows, numberOfCols, (r, c) => vector(r * numberOfRows + c))
  }

  def apply(matrix: Matrix): Matrix = Matrix(matrix.mat)

}
