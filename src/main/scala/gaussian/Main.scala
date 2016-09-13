package gaussian

import breeze.linalg.{Axis, DenseMatrix, DenseVector, eigSym, min}
import breeze.plot.{Figure, Plot}
import gaussian.Gaussian.{Location, MyNum}
import breeze.stats.{median}

object Main {

  def getTrainingAndTestSets(data: DenseMatrix[MyNum], dataSize: Int) : (DenseMatrix[MyNum], DenseMatrix[MyNum]) = {
    // now delete some rows from test data and some rows from training data
    var trainingSet = data(1 to dataSize, ::)
    var testSet = trainingSet.copy

    val testSetIndices = for {
      i <- 0 until dataSize
      if (i % 10 != 0)
    } yield i

    // remove test indices from training set
    trainingSet = trainingSet.delete(testSetIndices, Axis._0)

    val trainingSetIndices = for {
      i <- 0 until dataSize
      if (i % 10 == 0)
    } yield i

    // remove training indices from test set
    testSet = testSet.delete(trainingSetIndices, Axis._0)

    println(s"data split into training set of size ${trainingSet.rows} and test set of size ${testSet.rows}")

    (trainingSet, testSet)
  }

  def main(args: Array[String]): Unit = {
    val fileName = args(0)
    val dataSize = args(1).toInt

    val data = Utils.csvToMatrix(fileName)
    println(s"${data.rows} by ${data.cols}")

    val (trainingSet, testSet) = getTrainingAndTestSets(data, dataSize)

    println("calculating covariance")
    val kAndDist = Gaussian.getK(trainingSet)

    val medDistance = breeze.stats.median(kAndDist._2)

    println(s"median distance (use this for bandwidth param) = ${medDistance}")

    println("calculating eigen")
    val eigen = eigSym(kAndDist._1)
    println(s"smallest eigenvalue ${min(eigen.eigenvalues)}")

    println("calculating predictions for all values in test set")

    // long, lat, prediction, actual, error
    val predsForCSV = new DenseMatrix[MyNum](testSet.rows, 5)

    val predictions = for (i <- 0 until testSet.rows) {
      val testLoc = Location(data(i, 1), data(i, 0))
      val prediction = Gaussian.getPrediction(kAndDist._1, testLoc, trainingSet, 2)
      val actual = data(i, 2)
      val err: MyNum = math.abs((prediction.mean - actual) / actual)
      predsForCSV(i,0) = testLoc.long
      predsForCSV(i,1) = testLoc.lat
      predsForCSV(i,2) = prediction.mean
      predsForCSV(i,3) = actual
      predsForCSV(i,4) = err
      //println(s"predicted=$prediction, actual=$actual, error = $err")
    }

    /*val errVec = new DenseVector[MyNum](predictionErrors.toArray)
    val fig = Figure()
    val p: Plot = fig.subplot(0)
    p += breeze.plot.hist(errVec, 20)
    p.ylabel = "number of errors"
    p.xlabel = "error"
    p.xaxis.setTickLabelsVisible(true)
    p.title = s"Prediction errors: ${trainingSet.rows} training rows, ${testSet.rows} test rows"
    fig.saveas("prediction_errors.png")*/


    Utils.matrixToCSV(predsForCSV, "output.csv")
    println(s"results written to output.csv")

  }

}
