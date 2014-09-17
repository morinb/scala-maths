package org.bm.maths.matrix

import java.lang.Math.{abs, min}

/**
 *
 * @author Baptiste Morin
 */
case class LUDecomposition(matrix: Matrix) {
  val LU = matrix.copy
  val m: Int = LU.rowNumber
  val n: Int = LU.colNumber
  val piv: Array[Int] = Array.ofDim[Int](m)


  for {
    i <- 0 until m
  } yield piv(i) = i

  var pivSign: Int = 1
  var LUrowi: Array[Double] = Array()
  val LUcolj: Array[Double] = Array.ofDim[Double](m)

  // outer loop
  for (j <- 0 until n) {
    // make a copy of the j-th column to localize references.
    for {
      i <- 0 until m
    } yield LUcolj(i) = LU(i)(j)

    for (i <- 0 until m) {
      LUrowi = LU(i)

      // Most of the time is spent in the following dot product.

      val kmax = min(i, j)
      val sequence = for {
        k <- 0 until kmax
      } yield LUrowi(k) * LUcolj(k)

      val s = sequence.sum

      LUcolj(i) -= s
      LUrowi(j) = LUcolj(i)
    }

    // Find pivot and exchange if necessary.
    var p = j
    for (i <- j + 1 until m) {
      if (abs(LUcolj(i)) > abs(LUcolj(p))) {
        p = i
      }
    }

    if (p != j) {
      for (k <- 0 until n) {
        // swap
        val t = LU(p)(k)
        LU(p)(k) = LU(j)(k)
        LU(j)(k) = t
      }
      // swap
      val t = piv(p)
      piv(p) = piv(j)
      piv(j) = t
      pivSign = -pivSign
    }

    // compute multipliers
    if (j < m & LU(j)(j) != 0.0) {
      for {
        i <- j + 1 until m
      } yield LU(i)(j) /= LU(j)(j)
    }
  }

  def isNonSingular: Boolean = {
    for (j <- 0 until n) {
      if (LU(j)(j) == 0.0)
        return false
    }

    true
  }

  def L: Matrix = {
    val mat = Array.ofDim[Double](m, n)
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (i > j) {
          mat(i)(j) = LU(i)(j)
        } else if (i == j) {
          mat(i)(j) = 1.0
        } else {
          mat(i)(j) = 0.0
        }
      }
    }
    Matrix(mat)
  }

  def U: Matrix = {
    val mat = Array.ofDim[Double](m, n)
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (i <= j) {
          mat(i)(j) = LU(i)(j)
        } else {
          mat(i)(j) = 0.0
        }
      }
    }
    Matrix(mat)
  }

  def pivot: Array[Int] = {
    val p: Array[Int] = Array.ofDim[Int](m)
    for {
      i <- 0 until m
    } yield p(i) = piv(i)
    p
  }

  def doublePivot: Array[Double] = {
    val p: Array[Double] = Array.ofDim[Double](m)
    for {
      i <- 0 until m
    } yield p(i) = piv(i)
    p
  }

  def det: Double = {
    require(m == n, "Matrix must be square.")

    val seq = for {
      j <- 0 until n
    } yield LU(j)(j)

    pivSign * seq.product
  }

  def solve(that: Matrix): Matrix = {
    require(m == that.rowNumber, "Matrix row dimensions must agree")
    require(isNonSingular, "Matrix is singular")

    // copy right hand side with pivoting
    val nx = that.colNumber
    val X = that(piv, Array(0, nx - 1))

    // Solve L*Y = B(piv, :)
    for {
      k <- 0 until n
      i <- k + 1 until n
      j <- 0 until nx
    } yield X(i)(j) -= X(k)(j) * LU(i)(k)

    // Solve U*X = Y
    for (k <- n - 1 to 0 by -1) {
      for (j <- 0 until nx) {
        X(k)(j) /= LU(k)(k)
      }
      for (i <- 0 until k) {
        for (j <- 0 until nx) {
          X(i)(j) -= X(k)(j) * LU(i)(k)
        }
      }
    }
    X
  }

}
