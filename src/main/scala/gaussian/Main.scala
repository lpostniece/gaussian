package gaussian

import breeze.linalg.{DenseMatrix, eigSym, min}
import gaussian.Gaussian.Location
import breeze.stats.median

object Main {

  def main(args: Array[String]): Unit = {
    val fileName = args(0)
    val dataSize = args(1).toInt
    val testItemIndex = args(2).toInt

    val data = Utils.csvToMatrix(fileName)
    println(s"${data.rows} by ${data.cols}")

    // TODO: split data into training and test sets
    val trainingSet = data(1 to dataSize, ::)
    println("calculating covariance")
    val kAndDist = Gaussian.getK(trainingSet)
    //println(k)

    val medDistance = breeze.stats.median(kAndDist._2)

    println(s"median distance (use this for bandwidth param) = ${medDistance}")

    println("calculating eigen")
    val eigen = eigSym(kAndDist._1)
    println(s"smallest eigenvalue ${min(eigen.eigenvalues)}")

    println("calculating predictions")
    val testLoc = Location(data(testItemIndex, 1), data(testItemIndex, 0))
    val prediction = Gaussian.getPrediction(kAndDist._1, testLoc, trainingSet, 2)
    println(s"predicted=$prediction")
    println(s"actual=${data(testItemIndex, 2)}")
  }

}
