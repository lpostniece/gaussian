import breeze.linalg.{DenseMatrix, DenseVector, inv}
import math._

// implements first example of
// http://www.robots.ox.ac.uk/~mebden/reports/GPtutorial.pdf

type MyNum = Double // can change to Float if need be?

// params for model that may need tweaking
val l: MyNum = 1
val sigma_n = 0.3
val sigma_f = 1.27

def getSimplePaperTestMatrix = {
  val inputs = DenseMatrix.zeros[MyNum](6, 2)

  // fill in values from paper example
  inputs(0,0) = -1.5
  inputs(1,0) = -1
  inputs(2,0) = -0.75
  inputs(3,0) = -0.40
  inputs(4,0) = -0.25
  inputs(5,0) = 0
  inputs(0,1) = -1.7
  inputs(1,1) = -1.1
  inputs(2,1) = -0.3
  inputs(3,1) = 0.3
  inputs(4,1) = 0.6
  inputs(5,1) = 0.8

  inputs
}

// is this really just checking equality?
def kroneckerDelta(x: MyNum, xPrime: MyNum) = {
  if (x == xPrime) 1 else 0
}

def kfunc(x: MyNum, xPrime: MyNum) = {
  val minus = -1 * (pow((x - xPrime),2))/2
  val k = pow(sigma_f,2)*exp(minus) + pow(sigma_n,2)*kroneckerDelta(x, xPrime)
  k
}

def covarianceK(inputRow: DenseVector[MyNum]): DenseMatrix[MyNum] = {
  val len = inputRow.length
  val covarM = DenseMatrix.zeros[MyNum](len, len)

  for (i <- 0 until len) {
    for (j <- 0 until len) {
      val x = inputRow(i)
      val xPrime = inputRow(j)
      covarM(i, j) = kfunc(x, xPrime)
    }
  }

  covarM
}

def kStar(xStar: MyNum, inputRow: DenseVector[MyNum]): DenseVector[MyNum] = {
  val len = inputRow.length
  var kStarVector = DenseVector.zeros[MyNum](len)
  for (i <- 0 until len) {
    kStarVector(i) = kfunc(xStar, inputRow(i))
  }

  kStarVector
}

def kStarStar(xStar: MyNum) = kfunc(xStar,xStar)

val inputs = getSimplePaperTestMatrix
val firstColumn = inputs(::,0)
val k = covarianceK(firstColumn)
val kStarVec = kStar(0.2, firstColumn)
val kStarStarScalar = kStarStar(0.2)
println(kStarStarScalar)

val secondColumn = inputs(::,1)

/**
  * Computes the inverse of a given real matrix.
  * In general, you should avoid using this metho in combination with *.
  * Instead, wherever you might want to write inv(A) * B, you should write
  * A \ B.
  */

val kStarRow = kStarVec.asDenseMatrix
val yMean = kStarRow * (k \ secondColumn)

val yVarianceRHS = (kStarRow * breeze.linalg.inv(k)*kStarRow.t)
val yVariance = kStarStarScalar - yVarianceRHS

val y = yMean(0)
val variance = yVariance(0,0)