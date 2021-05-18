package foo

object KMeans{
    //This function calculates the difference between a point and a centroid
    //It returns a number between 0 and 1
    def calculateDistance(point: Array[Double], centroid: Array[Double], l: Int, r: Int, minValues: Array[Double], maxValues: Array[Double]): Double = {
        var sum: Double = 0
        if(r - l < 20){
            for(index <- l to r){
                var pointIndexValue = normalizeValues(point(index), index, minValues(index), maxValues(index))
                var centroidIndexValue = normalizeValues(centroid(index), index, minValues(index), maxValues(index))
                sum += (pointIndexValue - centroidIndexValue) * (pointIndexValue - centroidIndexValue)
            }
            sum
        }
        else{
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
}