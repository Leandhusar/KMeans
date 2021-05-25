package foo

import concurrencyCommon._
import randomCommon._
import KMeansBootstrapper._

object KMeans{
  def main(args: Array[String]): Unit = {
      //printArray(groups)
      //printCentroids(centroids)
      
      SSE = calculateNewSSE()

      calculateNearestCentroids()
      recalculateCentroids(0, centroids.length - 1)
      prevSSE = SSE
      SSE = calculateNewSSE()

      while((prevSSE - SSE) > epsilon){
          calculateNearestCentroids()
          recalculateCentroids(0, centroids.length - 1)
          prevSSE = SSE
          SSE = calculateNewSSE()
      }

      print(prevSSE, SSE)
      
      //printArray(groups)
      //printCentroids(centroids)
  }

  /*def run[B](block: => B) : Double = {
    val time = config (
      Key.exec.benchRuns -> 20
    ) withWarmer{
      new Warmer.Default
    } withMeasurer{
      new Measurer.IgnoringGC
    } measure{
      block
    }
    return time.value
  }*/
}