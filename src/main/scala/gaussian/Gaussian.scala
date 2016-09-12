package gaussian

import breeze.linalg.{DenseMatrix, DenseVector}
import math._
import Haversine._

object Gaussian {

  type MyNum = Double // can change to Float if need be?

  // eventually this case class will have more params like moisture/rain etc at location
  case class Location(lat: MyNum, long: MyNum)

  // TODO: tweak these params
  val l: MyNum = 1
  val sigma_n = 0.03
  val sigma_f = 1.27

  def kfunc(loc: Location, locPrime: Location) = {
    val distance = haversine(loc, locPrime)
    val minus = -1 * distance * distance
    //val k = pow(sigma_f,2)*exp(minus)
    val k = exp(minus)

    if (loc == locPrime)
      k + pow(sigma_n,2)
    else
      k
  }

  def covarianceK(data: DenseMatrix[MyNum], latColumn: Int, longColumn: Int): DenseMatrix[MyNum] = {
    val len = data.rows
    val covarM = DenseMatrix.zeros[MyNum](len, len)

    for (i <- 0 until len) {
      for (j <- 0 until len) {
        val locationI = Location(data(i,latColumn), data(i,longColumn))
        val locationJ = Location(data(j,latColumn), data(j,longColumn))
        covarM(i, j) = kfunc(locationI, locationJ)
      }
    }

    covarM
  }

  // first column is longitude
  // second column is latitude
  // third column is yield
  def getK(data: DenseMatrix[MyNum]): DenseMatrix[MyNum] =
    covarianceK(data, 1, 0)

  def getKStar(testLocation: Location,
               data: DenseMatrix[MyNum],
               kMatrix: DenseMatrix[MyNum]): DenseVector[MyNum] = {
    val len = data.rows
    val kStarVector = DenseVector.zeros[MyNum](len)
    for (i <- 0 until len) {
      kStarVector(i) = kfunc(testLocation, Location(data(i, 1), data(i, 0)))
    }

    kStarVector
  }

  def getKStarStar(testLocation: Location) = kfunc(testLocation, testLocation)

  case class Prediction(mean: MyNum, variance: MyNum)

  def getPrediction(kStarVec: DenseVector[MyNum],
                    k: DenseMatrix[MyNum],
                    kStarStarScalar: MyNum,
                    outputData: DenseVector[MyNum]): Prediction = {
    val kStarRow = kStarVec.asDenseMatrix
    val yMean = kStarRow * (k \ outputData)

    val yVarianceRHS = (kStarRow * breeze.linalg.inv(k)*kStarRow.t)
    val yVariance = kStarStarScalar - yVarianceRHS

    Prediction(yMean(0), yVariance(0,0))
  }

  def getPrediction(k: DenseMatrix[MyNum],
                    testLocation: Location,
                    trainingData: DenseMatrix[MyNum],
                    outputColumn: Int) : Prediction = {
    val kStar = getKStar(testLocation, trainingData, k)
    val kStarStar = getKStarStar(testLocation)
    getPrediction(kStar, k, kStarStar, trainingData(::, outputColumn))
  }
}
