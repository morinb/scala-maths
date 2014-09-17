package org.bm.maths.matrix

import java.lang.Math.{abs, max, min, pow, sqrt}

import scala.util.control.Breaks._

/**
 *
 * @author Baptiste Morin
 */
case class SingularValueDecomposition(matrix: Matrix) extends Maths {
  // Derived from LINPACK code.
  // Initialize.
  val A = matrix.copy
  val m = A.rowNumber
  val n = A.colNumber

  /* Apparently the failing cases are only a proper subset of (m<n),
	 * so let's not throw error.  Correct fix to come later?
   *   if (m<n) {
	 *     throw new IllegalArgumentException("SVD only works for m >= n");
	 *   }
   */
  val nu = min(m, n)
  val s = Array.ofDim[Double](min(m + 1, n))

  val Um = Matrix(m, nu)

  val Vm = Matrix(n, n)

  val e = Array.ofDim[Double](n)
  val work = Array.ofDim[Double](m)

  var wantu = true
  var wantv = true

  // Reduce A to bidiagonal form, storing the diagonal elements
  // in s and the super-diagonal elements in e.

  val nct = min(m - 1, n)
  val nrt = max(0, min(m, n - 2))

  for (k <- 0 until max(nct, nrt)) {
    if (k < nct) {
      // Compute the transformation for the k-th column and
      // place the k-th diagonal in s[k].
      // Compute 2-norm of k-th column without under/overflow.

      s(k) = 0
      for {i <- k until m} yield s(k) = hypot(s(k), A(i)(k))
      if (s(k) != 0.0) {
        if (A(k)(k) < 0.0) {
          s(k) = -s(k)
        }
        for {i <- k until m} yield A(i)(k) /= s(k)
        A(k)(k) += 1.0
      }
      s(k) = -s(k)
    }

    for (j <- k + 1 until n) {
      if ((k < nct) & (s(k) != 0.0)) {
        // Apply the transformation
        var t = (for {i <- k until m} yield A(i)(k) * A(i)(j)).sum
        t = -t / A(k)(k)
        for {i <- k until m} yield A(i)(j) += t * A(i)(k)
      }
      // Place the k-th row of A into e for the
      // subsequent calculation of the row transformation
      e(j) = A(k)(j)
    }
    if (wantu & (k < nct)) {
      //place the tansformation un U for subsequent back multiplication
      for {i <- k until m} yield Um(i)(k) = A(i)(k)
    }
    if (k < nrt) {
      // compute the k-th row transformation and place the
      // k-th super-diagonal in e[k].
      // Compute 2-norm without under/overflow
      e(k) = 0.0
      for {
        i <- k + 1 until n
      } yield e(k) = hypot(e(k), e(i))

      if (e(k) != 0.0) {
        if (e(k + 1) < 0.0) {
          e(k) = -e(k)
        }
        for {i <- k + 1 until n} yield e(i) /= e(k)
        e(k + 1) += 1.0
      }
      e(k) = -e(k)
      if ((k + 1 < m) & (e(k) != 0.0)) {
        // Apply the transformation
        for {i <- k + 1 until m} yield work(i) = 0.0
        for {
          j <- k + 1 until n
          i <- k + 1 until m
        } yield work(i) += e(j) * A(i)(j)

        for (j <- k + 1 until n) {
          val t = -e(j) / e(k + 1)
          for (i <- k + 1 until m) {
            A(i)(j) += t * work(i)
          }
        }
      }
      if (wantv) {
        // place the transformation in V for subsequence
        // back multiplication.
        for {i <- k + 1 until n} yield Vm(i)(k) = e(i)
      }
    }
  }

  // Set up the final bidiagonal matrix of order p
  var p = min(n, m + 1)
  if (nct < n) {
    s(nct) = A(nct)(nct)
  }
  if (m < p) {
    s(p - 1) = A(nrt)(p - 1)
  }
  if (nrt + 1 < p) {
    e(nrt) = A(nrt)(p - 1)
  }
  e(p - 1) = 0.0
  // If required, generate U
  if (wantu) {
    for (j <- nct until nu) {
      for (i <- 0 until m) {
        Um(i)(j) = 0.0
      }
      Um(j)(j) = 1.0
    }
    for (k <- nct - 1 to 0 by -1) {
      if (s(k) != 0.0) {
        for (j <- k + 1 until nu) {
          var t = (for {i <- k until m} yield Um(i)(k) * Um(i)(j)).sum
          t = -t / Um(k)(k)
          for {i <- k until m} yield Um(i)(j) += t * Um(i)(k)
        }
        for {i <- k until m} yield Um(i)(k) = -Um(i)(k)
        Um(k)(k) = 1.0 + Um(k)(k)
        for {i <- 0 until k - 1} yield Um(i)(k) = 0.0
      } else {
        for {i <- 0 until m} yield Um(i)(k) = 0.0
        Um(k)(k) = 1.0
      }
    }
  }
  // If required, generate V.
  if (wantv) {
    for (k <- n - 1 to 0 by -1) {
      if ((k < nrt) & (e(k) != 0.0)) {
        for (j <- k + 1 until nu) {
          var t = (for {i <- k + 1 until n} yield Vm(i)(k) * Vm(i)(j)).sum
          t = -t / Vm(k + 1)(k)
          for {i <- k + 1 until n} yield Vm(i)(j) += t * Vm(i)(k)
        }
      }
      for {i <- 0 until n} yield Vm(i)(k) = 0.0
      Vm(k)(k) = 1.0
    }
  }

  // Main iteration loop for the singular values

  val pp = p - 1
  var iter = 0
  val eps = pow(2.0, -52.0)
  val tiny = pow(2.0, -966.0)
  while (p > 0) {
    var kase = -1
    // Here is where a test for too many iterations would go.

    // This section of the program inspects for
    // negligible elements in the s and e arrays.  On
    // completion the variables kase and k are set as follows.

    // kase = 1     if s(p) and e[k-1] are negligible and k<p
    // kase = 2     if s(k) is negligible and k<p
    // kase = 3     if e[k-1] is negligible, k<p, and
    //              s(k), ..., s(p) are not negligible (qr step).
    // kase = 4     if e(p-1) is negligible (convergence).
    var k = p - 2
    breakable {
      while (k >= -1) {

        if (k == -1) {
          break()
        }
        if (abs(e(k)) <= tiny + eps * (abs(s(k)) + abs(s(k + 1)))) {
          e(k) = 0
          break()
        }
        k = k - 1
      }
    }
    if (k == p - 2) {
      kase = 4
    } else {
      var ks = p - 1
      breakable {
        while (ks >= k) {
          if (ks == k) {
            break()
          }
          val t = (if (ks != p) abs(e(ks)) else 0) + (if (ks != k + 1) abs(e(ks - 1)) else 0)
          if (abs(s(-ks)) <= tiny + eps * t) {
            s(ks) = 0.0
            break()
          }
          ks = ks - 1
        }
        if (ks == k) {
          kase = 3
        } else if (ks == p - 1) {
          kase = 1
        } else {
          kase = 2
          k = ks
        }
      }
    }
    k = k + 1

    // Perform the task indicated by kase

    k match {

      // Deflate negligible s(p).
      case 1 =>
        var f = e(p - 2)
        for (j <- p - 2 to k by -1) {
          var t = hypot(s(j), f)
          val cs = s(j) / t
          val sn = f / t
          s(j) = t
          if (j != k) {
            f = -sn * e(j - 1)
            e(j - 1) = cs * e(j - 1)
          }
          if (wantv) {
            for (i <- 0 until n) {
              t = cs * Vm(i)(j) + sn * Vm(i)(p - 1)
              Vm(i)(p - 1) = -sn * Vm(i)(j) + cs * Vm(i)(p - 1)
              Vm(i)(j) = t
            }
          }
        }

      // Split at negligible s(k).
      case 2 =>
        var f = e(k - 1)
        e(k - 1) = 0.0
        for (j <- k until p) {
          var t = hypot(s(j), f)
          val cs = s(j) / t
          val sn = f / t
          s(j) = t
          f = -sn * e(j)
          e(j) = cs * e(j)
          if (wantu) {
            for (i <- 0 until m) {
              t = cs * Um(i)(j) + sn * Um(i)(k - 1)
              Um(i)(k - 1) = -sn * Um(i)(j) + cs * Um(i)(k - 1)
              Um(i)(j) = t
            }
          }
        }

      // Perform one qr step.
      case 3 =>
        // Calculate the shift
        val scale = Math.max(max(max(max(
          abs(s(p - 1)), abs(s(p - 2))), abs(e(p - 2))),
          abs(s(k))), abs(e(k)))
        val sp = s(p - 1) / scale
        val spm1 = s(p - 2) / scale
        val epm1 = e(p - 2) / scale
        val sk = s(k) / scale
        val ek = e(k) / scale

        val b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0
        val c = (sp * epm1) * (sp * epm1)
        var shift = 0.0
        if ((b != 0.0) | (c != 0.0)) {
          shift = sqrt(b * b + c)
          if (b < 0.0) {
            shift = -shift
          }
          shift = c / (b + shift)
        }
        var f = (sk + sp) * (sk - sp) + shift
        var g = sk * ek

        // Chase zeros.
        for (j <- k until p - 1) {
          var t = hypot(f, g)
          var cs = f / t
          var sn = g / t
          if (j != k) {
            e(j - 1) = t
          }
          f = cs * s(j) + sn * e(j)
          e(j) = cs * e(j) - sn * s(j)
          g = sn * s(j + 1)
          s(j + 1) = cs * s(j + 1)
          if (wantv) {
            for (i <- 0 until n) {
              t = cs * Vm(i)(j) + sn * Vm(i)(j + 1)
              Vm(i)(j + 1) = -sn * Vm(i)(j) + cs * Vm(i)(j + 1)
              Vm(i)(j) = t
            }
          }
          t = hypot(f, g)
          cs = f / t
          sn = g / t
          s(j) = t
          f = cs * e(j) + sn * s(j + 1)
          s(j + 1) = -sn * e(j) + cs * s(j + 1)
          g = sn * e(j + 1)
          e(j + 1) = cs * e(j + 1)
          if (wantu && (j < m - 1)) {
            for (i <- 0 until m) {
              t = cs * Um(i)(j) + sn * Um(i)(j + 1)
              Um(i)(j + 1) = -sn * Um(i)(j) + cs * Um(i)(j + 1)
              Um(i)(j) = t
            }
          }
        }
        e(p - 2) = f
        iter = iter + 1

      // Convergence.
      case 4 =>
        // Make the singular values positive.
        if (s(k) <= 0.0) {
          s(k) = if (s(k) < 0.0) -s(k) else 0.0
          if (wantv) {
            for {i <- 0 to pp} yield Vm(i)(k) = -Vm(i)(k)
          }
        }

        // Order the singular values.
        breakable {
          while (k < pp) {
            if (s(k) >= s(k + 1)) {
              break()
            }
            var t = s(k)
            s(k) = s(k + 1)
            s(k + 1) = t
            if (wantv && (k < n - 1)) {
              for (i <- 0 until n) {
                t = Vm(i)(k + 1)
                Vm(i)(k + 1) = Vm(i)(k)
                Vm(i)(k) = t
              }
            }
            if (wantu && (k < m - 1)) {
              for (i <- 0 until m) {
                t = Um(i)(k + 1)
                Um(i)(k + 1) = Um(i)(k)
                Um(i)(k) = t
              }
            }
            k = k + 1
          }
          iter = 0
          p = p - 1
        }
    }
  }


  def U = Um

  def V = Vm

  def singularValues = s

  def S = Matrix(n, n, (i, j) => if (i == j) s(i) else 0.0)

  def norm2: Double = s(0)

  def cond: Double = s(0) / s(min(m, n) - 1)

  def rank: Int = {
    val eps = pow(2, -52)
    val tol = max(m, n) * s(0) * eps
    var r = 0
    for (i <- 0 until s.length) {
      if (s(i) > tol) {
        r = r + 1
      }
    }
    r
  }

}
