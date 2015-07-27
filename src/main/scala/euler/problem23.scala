package euler

import scala.collection.mutable._
import scala.math._
import BigInt._
import java.io._

// A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
// For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,
// which means that 28 is a perfect number.
// A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant
// if this sum exceeds n.
// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written
// as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater
// than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced
// any further by analysis even though it is known that the greatest number that cannot be expressed as
// the sum of two abundant numbers is less than this limit.
//
// Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
object Problem23 {
   def factorSum(num: Int): Int = {
    var factors = (for (i <- 2 to floor(sqrt(num)).toInt; if (num % i == 0)) yield List(i, num / i)).flatten.distinct
    (factors :+ 1).foldLeft(0) {_ + _}
  }

   def isAbundant(num: Int) = {
     factorSum(num) > num
   }
   
	def main(args: Array[String]) = {
    var abundantNumbers: HashSet[Int] = (for (i <- 1 to 28123; if isAbundant(i)) yield i)(collection.breakOut)
    //println(abundantNumbers)
    var sumsOfTwo = for (i <- 1 to 28123; aNum <- abundantNumbers;
        if (i > aNum && abundantNumbers.contains(i - aNum))) yield i
    //println(sumsOfTwo)
    var notSumsOfTwo = for (i <- 1 to 28123; if !sumsOfTwo.contains(i)) yield i
    //println(notSumsOfTwo)
    println("Problem 23: " + notSumsOfTwo.foldLeft(0) {_ + _})
  }
}
