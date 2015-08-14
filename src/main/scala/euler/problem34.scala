package euler

import scala.collection.mutable._
import scala.math._
import util.Time._

// 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
// Find the sum of all numbers which are equal to the sum of the factorial of their digits.
// Note: as 1! = 1 and 2! = 2 are not sums they are not included.

object Problem34 {
  var factorials = List(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
  
  def numToFactorialSum(num: Int): Int = {
    var n = num
    var sum = 0
    while (n > 0) {
      val digit = n % 10
      sum += factorials(digit)
      //println("digit: " + factorials(digit))
      n = n / 10
    }
     //println("sum: " + sum)
    sum
  }
  
  def main(args: Array[String]) = {
    time {
      var total = (3 to 10000000).filter(num => num == numToFactorialSum(num)).sum
      println("Problem 34: " + total)
    }
  }
}
