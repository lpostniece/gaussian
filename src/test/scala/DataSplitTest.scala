import breeze.linalg.{DenseMatrix, DenseVector}
import gaussian.Utils
import org.scalatest.FlatSpec

/**
  * Created by pos026 on 13/09/2016.
  */
class DataSplitTest extends FlatSpec {

  "split" should "produce disjoint sets" in {
    val entireSet = new DenseMatrix[Double](100, 1)
    for (i <- 0 until entireSet.rows) {
      entireSet(i, 0) = i
    }

    val split = Utils.getTrainingAndTestSets(entireSet, 100)

    val testSet = split._1(::,0).data
    val trainingSet = split._2(::,0).data

    // check that sets do not overlap
    for (testItem <- testSet) {
      assert(!(trainingSet contains testItem))
    }
  }

}
