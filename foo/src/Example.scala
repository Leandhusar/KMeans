package foo

import concurrencyCommon._
import randomCommon._
import KMeans._

object Example{
  def main(args: Array[String]): Unit = {
    var SSE: Double = 0.0
    var prevSSE: Double = 0.0

    //The dataset contains composite arrays of credit risk score, requested amount of credit, and age of users with pending orders
    var dataset = Array(
        Array(0.34, 250, 20), Array(0.65, 410, 30), Array(0.26, 310, 41), Array(0.61, 210, 53), Array(0.60, 250, 62),
        Array(0.44, 250, 41), Array(0.51, 340, 25)
    )

    //This variable stores the index of the centroid k to which point i is closest
    var groups = new Array[Int](dataset.length)
    
    var minData = new Array[Double](dataset(0).length)
    var maxData = new Array[Double](dataset(0).length)

    for(i <- 0 to (dataset(0).length - 1)){
        minData(i) = dataset(0)(i)
        maxData(i) = dataset(0)(i)
    }

    //It takes the maximum and minimum values ​​of each parameter of the dataset
    for(data <- dataset){
        for(i <- 0 to (data.length - 1)){
            if(data(i) < minData(i)){
                minData(i) = data(i)
            }
            if(data(i) > maxData(i)){
                maxData(i) = data(i)
            }
        }
    }

    /**
      * At this point the number of centroids and their associated points are defined
      * For practical purposes, the initial centroids will be assigned manually
      */
    var centroids = Array(
        Array(0.30, 210, 30),
        Array(0.45, 300, 45),
        Array(0.60, 400, 60)
    )

    for(i <- 0 to (dataset.length - 1)){
        var nearestCentroid: Int = 0
        var auxDistance: Double = 123456789.00
        for(k <- 0 to (centroids.length - 1)){
            var aux = calculateDistance(dataset(i), centroids(k), 0, dataset(i).length-1, minData, maxData)
            if(aux < auxDistance){
                auxDistance = aux
                nearestCentroid = k
            }
        }
        groups(i) = nearestCentroid
    }

    //Recalculate the centroids
    for(i <- 0 to (centroids.length - 1)){
        var auxCentroid = Array(0.0, 0.0, 0.0)
        var counter = 0
        for(k <- 0 to (groups.length - 1)){
            if(groups(k) == i){
                for(j <- 0 to (auxCentroid.length - 1)){
                    auxCentroid(j) = auxCentroid(j) + dataset(k)(j)
                }
                counter = counter + 1
            }
        }
        for(k <- 0 to (auxCentroid.length - 1)){
            centroids(i)(k) = auxCentroid(k) / counter
        }
        //prevSSE += calculateDistance(centroid(i), )
    }
  }
}