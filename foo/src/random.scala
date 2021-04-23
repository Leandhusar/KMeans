package foo

object randomCommon {
  def randIntValues(n: Int, max: Int = 100): Array[Int] = {
    Array.fill(n)(scala.util.Random.nextInt(max))
  }

  def randDoubleValues(n: Int): Array[Double] = {
    Array.fill(n)(scala.util.Random.nextDouble)
  }
}
