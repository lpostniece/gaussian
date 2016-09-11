package gaussian

import breeze.linalg.{DenseMatrix, DenseVector}
import math._
import Haversine._

object Gaussian {

  type MyNum = Double // can change to Float if need be?

  // TODO: tweak these params
  val l: MyNum = 1
  val sigma_n = 0.03
  val sigma_f = 1.27

  // is this really just checking equality?
  def kroneckerDelta(lat: MyNum, long: MyNum, latPrime: MyNum, longPrime: MyNum) = {
    if (lat == latPrime && long == longPrime) 1 else 0
  }

  def kfunc(lat: MyNum, long: MyNum, latPrime: MyNum, longPrime: MyNum) = {
    val distance = haversine(lat, long, latPrime, longPrime)
    val minus = -1 * distance * distance
    //val k = pow(sigma_f,2)*exp(minus)
    val k = exp(minus)

    if (lat == latPrime && long == longPrime)
      k + pow(sigma_n,2)
    else
      k
  }

  def covarianceK(inputLats: DenseVector[MyNum], inputLongs: DenseVector[MyNum]): DenseMatrix[MyNum] = {
    val len = inputLats.length // assumes lats and longs are of same length
    val covarM = DenseMatrix.zeros[MyNum](len, len)

    for (i <- 0 until len) {
      for (j <- 0 until len) {
        val lat = inputLats(i)
        val latPrime = inputLats(j)
        val long = inputLongs(i)
        val longPrime = inputLongs(j)
        covarM(i, j) = kfunc(lat, long, latPrime, longPrime)
      }
    }

    covarM
  }

  // first column is longitude
  // second column is latitude
  // third column is yield
  def getK(data: DenseMatrix[MyNum]): DenseMatrix[MyNum] =
    covarianceK(data(::, 1), data(::, 0))

  def getKStar(latStar: MyNum, longStar: MyNum,
               data: DenseMatrix[MyNum],
               kMatrix: DenseMatrix[MyNum]): DenseVector[MyNum] = {
    val len = data(::, 1).length
    val kStarVector = DenseVector.zeros[MyNum](len)
    for (i <- 0 until len) {
      kStarVector(i) = kfunc(latStar, longStar, data(i, 1), data(i, 0))
    }

    kStarVector
  }

  def getKStar(latStar: MyNum, longStar: MyNum,
               inputLats: DenseVector[MyNum], inputLongs: DenseVector[MyNum],
               kMatrix: DenseMatrix[MyNum]): DenseVector[MyNum] = {
    val len = inputLats.length
    val kStarVector = DenseVector.zeros[MyNum](len)
    for (i <- 0 until len) {
      kStarVector(i) = kfunc(latStar, longStar, inputLats(i), inputLongs(i))
    }

    kStarVector
  }

  def getKStarStar(latStar: MyNum, longStar: MyNum) =
    kfunc(latStar,longStar, latStar, longStar)

  case class Prediction(mean: MyNum, variance: MyNum)

  def getPrediction(kStarVec: DenseVector[MyNum],
                    k: DenseMatrix[MyNum],
                    kStarStarScalar: MyNum,
                    data: DenseMatrix[MyNum]): Prediction = {
    val kStarRow = kStarVec.asDenseMatrix
    val yMean = kStarRow * (k \ data(::, 2))

    val yVarianceRHS = (kStarRow * breeze.linalg.inv(k)*kStarRow.t)
    val yVariance = kStarStarScalar - yVarianceRHS

    Prediction(yMean(0), yVariance(0,0))
  }
}
