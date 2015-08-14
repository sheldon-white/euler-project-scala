package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// A googol (10100) is a massive number: one followed by one-hundred zeros;
// 100^100 is almost unimaginably large: one followed by two-hundred zeros.
// Despite their size, the sum of the digits in each number is only 1.
// Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?
object Problem56 {
  def power(num: Int, power: Int): BigInt = {
    val powerBits = power.toBinaryString.toList.reverse
    var accum: BigInt = 1
    var multiplier: BigInt = num
    //println(powerBits)
    for (i <- 0 until powerBits.length) {
      if (powerBits(i) == '1') {
        accum = accum * multiplier
      }
      multiplier = multiplier * multiplier
    }
    accum
  }
  
  def digitSum(num: BigInt) = num.toString.toList.map(d => d - '0').sum
  
  def main(args: Array[String]) = {
    time {
      val maxSum = (for (a <- 1 until 100; b <- 1 until 100) yield digitSum(power(a, b))).max
      println("Problem 56: " + maxSum)
    }
  }
}
