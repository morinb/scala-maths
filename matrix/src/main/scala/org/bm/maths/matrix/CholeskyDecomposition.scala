package org.bm.maths.matrix

import java.lang.Math.{max, sqrt}

/**
 *
 * @author Baptiste Morin
 */
case class CholeskyDecomposition(matrix: Matrix) {
  val A = matrix.copy
  val n = matrix.rowNumber

  // Internal storage of decomposition
  val Lstorage = Matrix(n, n)
  /**
   * Symmetric and positive definite flag
   */
  var isspd = matrix.colNumber == n

  // Main loop
  for (j <- 0 until n) {
    val Lrowj = Lstorage(j)
    var d = 0.0
    for (k <- 0 until j) {
      val Lrowk = Lstorage(k)

      var s = (for {
        i <- 0 until k
      } yield Lrowk(i) * Lrowj(i)
        ).sum

      s = (A(j)(k) - s) / Lstorage(k)(k)
      Lrowj(k) = s
      d = d + s * s
      isspd = isspd & (d > 0.0)
    }
    d = A(j)(j) - d
    isspd = isspd & (d > 0.0)
    Lstorage(j)(j) = sqrt(max(d, 0.0))
  }

  def isSPD = isspd

  def L = Lstorage

  def solve(B: Matrix): Matrix = {
    require(B.rowNumber == n, "Matrix row dimensions must agree.")
    require(isspd, "Matrix is not symmetric positive definite")

    // Copy right hand side
    val X = B.copy
    val nx = B.colNumber

    // Solve L*Y = B
    for (k <- 0 until n) {
      for (j <- 0 until nx) {
        for (i <- 0 until k) {
          X(k)(j) -= X(i)(j) * L(k)(i)
        }
        X(k)(j) /= L(k)(k)
      }
    }

    // Solve L'*X = Y
    for(k <- n-1 to 0 by -1) {
      for(j <- 0 until nx) {
        for(i <- k+1 until n) {
          X(k)(j) -= X(i)(j)*L(i)(k)
        }
        X(k)(j) /= L(k)(k)
      }
    }

    X


  }

}
