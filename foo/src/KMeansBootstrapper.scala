package foo

import concurrencyCommon._
import Data._

object KMeansBootstrapper{
    var epsilon = 0.0
    var SSE: Double = 0.0
    var prevSSE: Double = 0.0

    //The dataset contains composite arrays of credit risk score, requested amount of credit, and age of users with pending orders
    var dataset: Array[Array[Double]] = readCSV()
    
    //This variable stores the index of the centroid k to which point i is closest
    var groups = new Array[Int](dataset.length)
    
    var minData = new Array[Double](dataset(0).length)
    var maxData = new Array[Double](dataset(0).length)
    for(i <- 0 to (dataset(0).length - 1)){
            minData(i) = dataset(0)(i)
            maxData(i) = dataset(0)(i)
    }

    getMinMaxData()

    /**
      * At this point the number of centroids and their associated points are defined
      * For practical purposes, the initial centroids will be assigned manually
      */
    var centroids = Array(
        Array(41.0, 0.0, 1.0, 130.0, 204.0, 0.0, 0.0, 172.0, 0.0, 1.4, 2.0, 0.0, 2.0, 1.0),
        Array(44.0, 1.0, 2.0, 140.0,235.0,0.0,0.0,180.0,0.0,0.0,2.0,0.0,2.0,1.0),
        Array(36.0,1.0,3.0,135.0,250.0,0.0,1.0,187.0,0.0,3.5,0.0,0.0,2.0,1.0),
        Array(76.0,0.0,2.0,140.0,197.0,0.0,2.0,116.0,0.0,1.1,1.0,0.0,2.0,1.0),
        Array(56.0,0.0,0.0,200.0,288.0,1.0,0.0,133.0,1.0,4.0,0.0,2.0,3.0,0.0),
        Array(60.0,1.0,2.0,145.0,264.0,0.0,1.0,132.0,0.0,1.2,1.0,0.0,3.0,0.0),
        Array(36.0,1.0,3.0,151.0,200.0,0.0,1.0,117.0,0.0,3.0,0.0,0.0,2.0,1.0),
        Array(65.0,0.0,2.0,140.0,197.0,0.0,2.0,116.0,0.0,1.1,1.0,0.0,2.0,1.0),
        Array(40.0,0.0,0.0,200.0,288.0,1.0,0.0,133.0,1.0,4.0,0.0,2.0,3.0,0.0),
        Array(30.0,1.0,3.0,145.0,214.0,0.0,1.0,120.0,0.0,1.2,1.0,0.0,3.0,1.0)
    )

    calculateNearestCentroids()

    //Reinitialize centroids, groups and SSE
    def reinitialize(){
        groups = new Array[Int](dataset.length)
        centroids = initCentroids
        SSE = 0.0
        prevSSE = 0.0
    }

    //Recalculate the centroids
    def recalculateCentroids(l: Int, r: Int){
        if(r - l < 5){
            for(i <- l to r){
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
            }
        }
        else{
            parallel(recalculateCentroids(l, (r / 2)), recalculateCentroids((r / 2) + 1, r))
        }
    }

    def calculateNewSSE(): Double = {
        var counter = 0.0
        for(centroid <- centroids){
            for(point <- dataset){
                counter += calculateDistance(point, centroid, 0, point.length - 1, minData, maxData)
            }
        }
        counter
    }

    def calculateNearestCentroids(){
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
    }



    //It takes the maximum and minimum values ​​of each parameter of the dataset
    def getMinMaxData(){
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
    }

    //This function calculates the difference between a point and a centroid
    //It returns a number between 0 and 1
    def calculateDistance(point: Array[Double], centroid: Array[Double], l: Int, r: Int, minValues: Array[Double], maxValues: Array[Double]): Double = {
        var sum: Double = 0
        if(r - l < 50){
            for(index <- l to r){
                var pointIndexValue = normalizeValues(point(index), index, minValues(index), maxValues(index))
                var centroidIndexValue = normalizeValues(centroid(index), index, minValues(index), maxValues(index))
                sum += (pointIndexValue - centroidIndexValue) * (pointIndexValue - centroidIndexValue)
            }
            sum
        }
        else{
            //parallel(calculateDistance(point, centroid, l, (r/2), minValues, maxValues), calculateDistance(point, centroid, (r/2)+1, r, minValues, maxValues))
            sum += calculateDistance(point, centroid, l, (r/2), minValues, maxValues)
            sum += calculateDistance(point, centroid, (r/2)+1, r, minValues, maxValues)
            sum
        }
    }

    def normalizeValues(point: Double, index: Int, minValue: Double, maxValue: Double): Double = {
        var valuePrime: Double = (point - minValue) / (maxValue - minValue)
        valuePrime
    }

    def printArray(array: Array[Double]){
        for(i <- 0 to (array.length - 1)){
            println(array(i))
        }
    }

    def printArray(array: Array[Int]){
        for(i <- 0 to (array.length - 1)){
            println(array(i))
        }
    }

    def printCentroids(array: Array[Array[Double]]){
        for(element <- array){
            printArray(element)
        }
    }
}