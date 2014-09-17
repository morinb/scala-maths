package org.bm.maths.matrix

/**
 *
 * @author Baptiste Morin
 */
case class QRDecomposition(matrix: Matrix) extends Maths {
  // Initialize
  val QR = matrix.copy
  val m = QR.rowNumber
  val n = QR.colNumber
  val Rdiag = Array.ofDim[Double](n)

  // Main loop
  for (k <- 0 until n) {
    // Compute 2-norm of the k-th column without under/overflow
    var nrm = 0.0
    for (i <- k until m) {
      nrm = hypot(nrm, QR(i)(k))
    }

    if (nrm != 0.0) {
      // Form k-th Householder vector
      if (QR(k)(k) < 0.0) {
        nrm = -nrm
      }
      for (i <- k until m) {
        QR(i)(k) /= nrm
      }
      QR(k)(k) += 1.0

      // Apply transformation to remaining columns.
      for (j <- k + 1 until n) {
        var s = (for {
          i <- k until m
        } yield QR(i)(k) * QR(i)(j)
          ).sum

        s = -s / QR(k)(k)
        for {
          i <- k until m
        } yield QR(i)(j) += s * QR(i)(k)
      }
    }
    Rdiag(k) = -nrm
  }

  def isFullRank: Boolean = {
    for (j <- 0 until n) {
      if (Rdiag(j) == 0)
        return false
    }
    true
  }

  def H: Matrix = Matrix(m, n, (i, j) => if (i >= j) QR(i)(j) else 0.0)

  def R: Matrix = Matrix(m, n, (i, j) => if (i < j) QR(i)(j) else if (i == j) Rdiag(i) else 0.0)

  def Q: Matrix = {
    val X = Matrix(m, n)
    for (k <- n - 1 to 0 by -1) {
      for (i <- 0 until m) {
        X(i)(k) = 0.0
      }
      X(k)(k) = 1.0
      for (j <- k until n) {
        if (QR(k)(k) != 0.0) {
          var s = (for {
            i <- k until m
          } yield QR(i)(k) * X(i)(j)
            ).sum
          s = -s / QR(k)(k)
          for {
            i <- k until m
          } yield X(i)(j) += s * QR(i)(k)
        }
      }
    }
    X
  }

  def solve(B: Matrix): Matrix = {
    require(B.rowNumber == m, "Matrix row dimensions must agree.")
    require(isFullRank, "Matrix is rank deficient.")

    // Copy right hand side
    val nx = B.colNumber
    val X = B.copy

    // Compute Y = Qt * B
    for (k <- 0 until n) {
      for (j <- 0 until nx) {
        var s = (for {
          i <- k until m
        } yield QR(i)(k) * X(i)(j)).sum
        s = -s / QR(k)(k)
        for {
          i <- k until m
        } yield X(i)(j) += s * QR(i)(k)
      }
    }
    // Solve R*X = Y
    for (k <- n - 1 to 0 by -1) {
      for {j <- 0 until nx} yield X(k)(j) /= Rdiag(k)
      for {
        i <- 0 until k
        j <- 0 until nx
      } yield X(i)(j) -= X(k)(j) * QR(i)(k)
    }

    Matrix(X)((0, n - 1), (0, nx - 1))

  }

}
