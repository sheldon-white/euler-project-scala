package euler

import scala.collection.immutable._
import scala.math._
import BigInt._

//Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
//If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
//
//For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
//
//Evaluate the sum of all the amicable numbers under 10000.

object Problem21 {
  def factorSum(num: Int): Int = {
    var factors = (for (i <- 2 to floor(sqrt(num)).toInt; if (num % i == 0)) yield List(i, num / i)).flatten.distinct
    (factors :+ 1).foldLeft(0) {_ + _}
  }
  
	def main(args: Array[String]) = {
    var anTotal = 0
    for (num <- 1 to 10000) {
      var sum = factorSum(num)
      if (sum != num && factorSum(sum) == num) {
        //println(num + " and " + sum + " are amiable")
        anTotal += num
      }
    }
    println("Problem 21: " + anTotal)
  }
}
