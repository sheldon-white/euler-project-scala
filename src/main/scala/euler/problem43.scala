package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._

// The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order,
// but it also has a rather interesting sub-string divisibility property.
// Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
// d2d3d4=406 is divisible by 2
// d3d4d5=063 is divisible by 3
// d4d5d6=635 is divisible by 5
// d5d6d7=357 is divisible by 7
// d6d7d8=572 is divisible by 11
// d7d8d9=728 is divisible by 13
// d8d9d10=289 is divisible by 17
// Find the sum of all 0 to 9 pandigital numbers with this property.

object Problem43 {
  def matchingPermutations = ((for (i <- 0 to 9) yield i).permutations) map {_.mkString("").toLong} filter {_ > 1000000000} filter {hasProperty}

  def hasProperty(num: Long): Boolean = {
    val numStr = num.toString
    val sub1 = numStr.substring(1, 4).toInt
    val sub2 = numStr.substring(2, 5).toInt
    val sub3 = numStr.substring(3, 6).toInt
    val sub4 = numStr.substring(4, 7).toInt
    val sub5 = numStr.substring(5, 8).toInt
    val sub6 = numStr.substring(6, 9).toInt
    val sub7 = numStr.substring(7, 10).toInt
    (sub1 % 2 == 0 &&
    sub2 % 3 == 0 &&
    sub3 % 5 == 0 &&
    sub4 % 7 == 0 &&
    sub5 % 11 == 0 &&
    sub6 % 13 == 0 &&
    sub7 % 17 == 0)
  }
  
  def main(args: Array[String]) = {
    val solution = matchingPermutations.sum
    for (p <- matchingPermutations) {
      println(p)
    }
    println("Problem 43: " + solution)
  }
}
