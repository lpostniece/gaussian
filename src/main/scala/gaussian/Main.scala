package gaussian

import breeze.linalg.{DenseMatrix, eigSym, min}
import gaussian.Gaussian.Location

object Main {

  def main(args: Array[String]): Unit = {
    val fileName = args(0)
    val dataSize = args(1).toInt
    val testItemIndex = args(2).toInt

    val data = Utils.csvToMatrix(fileName)
    println(s"${data.rows} by ${data.cols}")

    // TODO: split data into training and test sets
    val trainingSet = data(1 to dataSize, ::)
    //println(s"${trainingSet.rows} by ${trainingSet.cols}")
    //println(trainingSet)

    println("calculating covariance")
    val k = Gaussian.getK(trainingSet)
    //println(k)

    println("calculating eigen")
    val eigen = eigSym(k)
    println(s"smallest eigenvalue ${min(eigen.eigenvalues)}")

    println("calculating predictions")
    val testLoc = Location(data(testItemIndex, 1), data(testItemIndex, 0))
    val prediction = Gaussian.getPrediction(k, testLoc, trainingSet, 2)
    println(s"predicted=$prediction")
    println(s"actual=${data(testItemIndex, 2)}")
  }

}
