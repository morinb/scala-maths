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

import java.lang.Math.{abs, max, min, pow, sqrt}
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


  def deep = this.mat.deep

  def copy = Matrix(mat)

  def apply(r: Int, c: Int) = mat(r)(c)

  def apply(r: Int): Array[Double] = mat(r)

  def apply(upperLeftCorner: (Int, Int), lowerRightCorner: (Int, Int)): Matrix = submatrix(upperLeftCorner, lowerRightCorner)

  def apply(rowsToKeep: Array[Int], colsToKeep: Array[Int]): Matrix = submatrix(rowsToKeep, colsToKeep)

  def apply(r: Array[Int], j0: Int, j1: Int): Matrix = submatrix(r, j0, j1)

  def update(row: Int, col: Int, value: Double): Unit = mat(col)(row) = value

  def update(rows: (Int, Int), cols: (Int, Int), m: Matrix): Unit = setSubMatrix(rows, cols, m)

  def ==(that: Matrix): Boolean = this equals that

  override def equals(other: Any): Boolean = other match {
    case that: Matrix => this.deep == that.deep
    case _ => false
  }

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

  def :+(that: Matrix): Matrix = {
    require(this.rowNumber == that.rowNumber, "Matrices must have the same number of row")

    val rowN = rowNumber
    val colN = colNumber + that.colNumber

    val m = Array.ofDim[Double](rowN, colN)

    for {
      r <- 0 until rowNumber
      c <- 0 until colNumber
    } yield m(r)(c) = this(r)(c)

    for {
      r <- 0 until rowN
      c <- colNumber until colN
    } yield m(r)(c) = that(r)(c - colNumber)

    Matrix(m)
  }

  def norm1: Double = {
    var f = 0.0
    for (j <- 0 until colNumber) {
      var s = 0.0
      for (i <- 0 until rowNumber) {
        s += abs(this(i)(j))
      }
      f = max(f, s)
    }
    f
  }

  def norm2: Double = svd.norm2

  /**
   * Infinity norm : maximum row sum
   * @return
   */
  def normInf = (
    for {
      r <- 0 until rowNumber
    } yield this(r)
    ).map(a => a.sum).max

  /**
   * Frobenius Norm : square root of sum of squares of all elements
   * @return
   */
  def normF: Double =
    sqrt((for {
      r <- 0 until rowNumber
      c <- 0 until colNumber
    } yield pow(this(r)(c), 2)
      ).sum)


  def +:(that: Matrix): Matrix = that :+ this

  def :+(d: Double): Matrix = Matrix(this).map(_ + d)

  def +:(d: Double): Matrix = this :+ d

  def :-(d: Double): Matrix = Matrix(this).map(_ - d)

  def -:(d: Double): Matrix = this :- d

  def :*(d: Double): Matrix = Matrix(this).map(_ * d)

  def *:(d: Double): Matrix = this :* d

  def :/(d: Double): Matrix = Matrix(this).map(_ / d)

  def /:(d: Double): Matrix = this :/ d

  def :\(d: Double): Matrix = Matrix(this).map(d / _)

  def \:(d: Double): Matrix = this :\ d

  def det: Double = lu.det

  def rank: Int = svd.rank

  def cond: Double = svd.cond

  /**
   * sum of diagonal elements
   * @return
   */
  def trace: Double = (for {
    i <- 0 until min(rowNumber, colNumber)
  } yield this(i, i)).sum

  def lu: LUDecomposition = LUDecomposition(this)

  def qr: QRDecomposition = QRDecomposition(this)

  def chol: CholeskyDecomposition = CholeskyDecomposition(this)

  def svd: SingularValueDecomposition = SingularValueDecomposition(this)

  def eig: EigenValueDecomposition = EigenValueDecomposition(this)

  def unary_- : Matrix = Matrix(rowNumber, colNumber, (r, c) => -this(r)(c))

  def +(that: Matrix): Matrix = {
    checkSize(that)
    Matrix(rowNumber, colNumber, (r, c) => this(r)(c) + that(r)(c))
  }

  def -(that: Matrix): Matrix = {
    checkSize(that)
    Matrix(rowNumber, colNumber, (r, c) => this(r)(c) - that(r)(c))
  }

  def *(that: Matrix): Matrix = {
    require(colNumber == that.rowNumber, "Matrix inner dimensions must agree.")

    val X: Matrix = Matrix(rowNumber, that.colNumber)
    val thatColj = Array.ofDim[Double](that.rowNumber)

    for (j <- 0 until that.colNumber) {
      for (k <- 0 until colNumber) {
        thatColj(k) = that(k)(j)
      }
      for (i <- 0 until rowNumber) {
        val thisRowi = this(i)
        var s: Double = 0.0
        for (k <- 0 until colNumber) {
          s += thisRowi(k) * thatColj(k)
        }
        X(i)(j) = s
      }
    }

    X
  }

  def inv: Matrix = solve(Matrix.identity(rowNumber, rowNumber))

  /**
   * transpose
   * @return
   */
  def t: Matrix = Matrix(colNumber, rowNumber, (r, c) => this(c)(r))


  def solve(that: Matrix): Matrix = if (rowNumber == colNumber) lu.solve(that) else qr.solve(that)

  def solveTranspose(that: Matrix) = t.solve(that.t)

  import org.bm.maths.matrix.Matrix.Implicits.numberFormat

  override def toString = asString()

  def asString()(implicit nf: NumberFormat) = {
    val sb: StringBuilder = new StringBuilder()
    mat.foreach(doubleArray =>
      sb append doubleArray.map(d => nf.format(d)).mkString("[", "  ", "]\n")
    )
    sb.toString()
  }


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

  private[matrix] def submatrix(rows: (Int, Int), cols: (Int, Int)): Matrix = {
    val startRow = rows._1
    val endRow = rows._2
    val startCol = cols._1
    val endCol = cols._2

    require(startRow <= endRow, "Start row must be lower or equal than end row")
    require(startCol <= endCol, "Start column must be lower or equal than end column")

    val rowN = endRow - startRow + 1
    val colN = endCol - startCol + 1

    val m = Array.ofDim[Double](rowN, colN)

    for {
      r <- startRow to endRow
      c <- startCol to endCol
    } yield m(r - startRow)(c - startCol) = mat(r)(c)

    Matrix(m)
  }

  private[matrix] def submatrix(rowsToKeep: Array[Int], colsToKeep: Array[Int]): Matrix = {
    val rowN = rowsToKeep.length
    val colN = colsToKeep.length

    val m = Array.ofDim[Double](rowN, colN)

    for {
      r <- 0 until rowN
      c <- 0 until colN
    } yield m(r)(c) = this(rowsToKeep(r))(colsToKeep(c))

    Matrix(m)
  }

  private[matrix] def submatrix(r: Array[Int], j0: Int, j1: Int): Matrix = {
    val X = Matrix(r.length, j1 - j0 + 1)

    try {
      for {
        i <- 0 until r.length
        j <- j0 to j1
      } yield X(i)(j - j0) = this(r(i))(j)
    } catch {
      case e: ArrayIndexOutOfBoundsException => throw new ArrayIndexOutOfBoundsException("Submatrix indices")
    }
    X
  }

  private[matrix] def checkSize(that: Matrix) {
    require(rowNumber == that.rowNumber, "Matrices must have same number of rows")
    require(colNumber == that.colNumber, "Matrices must have same number of columns")
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
    val colNumber = array(0).length

    Matrix(rowNumber, colNumber, (r, c) => array(r)(c))
  }

  def apply(vector: Array[Double], numberOfRows: Int): Matrix = {
    require((vector.length % numberOfRows) == 0, s"Vector length must be a multiple of $numberOfRows.")

    val numberOfCols = vector.length / numberOfRows
    Matrix(numberOfRows, numberOfCols, (r, c) => vector(r * numberOfRows + c))
  }

  def apply(matrix: Matrix): Matrix = Matrix(matrix.mat)

}
