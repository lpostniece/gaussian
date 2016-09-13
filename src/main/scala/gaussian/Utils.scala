package gaussian

import java.io.File

import breeze.linalg.{Axis, DenseMatrix, csvread, csvwrite}
import gaussian.Gaussian.MyNum



object Utils {

  val clazz = getClass()

  def csvToMatrix(csvFileName: String): DenseMatrix[MyNum] = {
    val matrix=csvread(new File(csvFileName),',', '"', 0)
    matrix
  }

  def matrixToCSV(matrix: DenseMatrix[MyNum], csvFileName: String) = {
    csvwrite(new File(csvFileName), matrix)
  }

  def getTrainingAndTestSets(data: DenseMatrix[MyNum], dataSize: Int) : (DenseMatrix[MyNum], DenseMatrix[MyNum]) = {
    // now delete some rows from test data and some rows from training data
    var trainingSet = data(0 until dataSize, ::)
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

}
