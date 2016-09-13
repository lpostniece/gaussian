package gaussian

import breeze.linalg.{Axis, DenseMatrix, DenseVector, eigSym, min}
import breeze.plot.{Figure, Plot}
import gaussian.Gaussian.{Location, MyNum}
import breeze.stats.median
import org.jfree.chart.axis.NumberTickUnit

object Main {



  def main(args: Array[String]): Unit = {
    val fileName = args(0)
    val dataSize = args(1).toInt

    val data = Utils.csvToMatrix(fileName)
    println(s"${data.rows} by ${data.cols}")

    val (trainingSet, testSet) = Utils.getTrainingAndTestSets(data, dataSize)

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

    val errVec = predsForCSV(::, 4)
    val fig = Figure()
    val p: Plot = fig.subplot(0)
    p += breeze.plot.hist(errVec, 20)
    p.ylabel = "number of errors"
    p.xlabel = "error"
    p.xaxis.setTickUnit(new NumberTickUnit(0.01))
    p.title = s"Prediction errors: ${trainingSet.rows} training rows, ${testSet.rows} test rows"
    fig.saveas("prediction_errors.png")

    Utils.matrixToCSV(predsForCSV, "output.csv")
    println(s"results written to output.csv")

  }

}
