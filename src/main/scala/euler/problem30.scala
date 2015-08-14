package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._
import util.Time._

// Problem 30: Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
// maximum possible sum = 5 * 9^5 = 295245
object Problem30 {
  def sumOfDigitPowers(i: Int): Int =
  {
    i.toString.toList.map {_.toString.toInt} map { pow(_, 5).toInt } reduceLeft { _ + _ }
  }
  
  def main(args: Array[String]) = {
    time {
      var list = for (i <- 2 to 295245; if i == sumOfDigitPowers(i)) yield i
      println("Problem 30: " + list.sum)
    }
  }
}
