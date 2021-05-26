package foo

import concurrencyCommon._
import randomCommon._
import Data._
import KMeansBootstrapper._

object KMeans{
  def main(args: Array[String]): Unit = {
    for(iteration <- 0 to 30){
      time(kMeans())
      reinitialize()
    }
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + " ns")
    result
  }

  def kMeans(){
      //printArray(groups)
      //printCentroids(centroids)
      
      SSE = calculateNewSSE()

      calculateNearestCentroids(0, centroids.length - 1)
      recalculateCentroids(0, centroids.length - 1)
      prevSSE = SSE
      SSE = calculateNewSSE()

      while((prevSSE - SSE) > epsilon){
          calculateNearestCentroids(0, centroids.length - 1)
          recalculateCentroids(0, centroids.length - 1)
          prevSSE = SSE
          SSE = calculateNewSSE()
      }
      
      //printArray(groups)
      //printCentroids(centroids)
  }
}