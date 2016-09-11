package gaussian

import java.io.File

import breeze.linalg.DenseMatrix
import gaussian.Gaussian.MyNum
import breeze.linalg.csvread



object Utils {

  val clazz = getClass()

  def csvToMatrix(csvFileName: String): DenseMatrix[MyNum] = {
    val matrix=csvread(new File(csvFileName),',', '"', 0)

    matrix
  }

}
