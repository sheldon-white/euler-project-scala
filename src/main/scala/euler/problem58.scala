package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
//
// 37 36 35 34 33 32 31
// 38 17 16 15 14 13 30
// 39 18  5  4  3 12 29
// 40 19  6  1  2 11 28
// 41 20  7  8  9 10 27
// 42 21 22 23 24 25 26
// 43 44 45 46 47 48 49
//
// It is interesting to note that the odd squares lie along the bottom right diagonal,
// but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime;
// that is, a ratio of 8/13 ≈ 62%.
// If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
// If this process is continued, what is the side length of the square spiral for which the ratio of primes
// along both diagonals first falls below 10%?
object Problem58 {
  def corners(n: Int): Vector[Long] = {
    val s: Long = 4*n*n + 4*n + 1
    Vector(s, s - 2*n, s - 4*n, s - 6*n).distinct
  }
    
  def firstMatch: Int = {
    var cornerCount = 1
    var primeCount = 0
    for (i <- 1 to 100000) {
      val c = corners(i)
      println(c)
      cornerCount += 4
      primeCount += (c filter {Prime.isPrime(_)}).size
      println(cornerCount, primeCount)
      if ((primeCount.toDouble / cornerCount) < 0.1) {
        return 2*i + 1
      }
    }
    0
  }
    
  def main(args: Array[String]) = {
    time {
      println("Problem 58: " + firstMatch)
    }
  }
}