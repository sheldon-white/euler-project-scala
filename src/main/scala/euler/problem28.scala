package euler

import scala.collection.mutable._
import scala.math._
import BigInt._
import java.io._
import util.Time._

// Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
//
// 21 22 23 24 25
// 20  7  8  9 10
// 19  6  1  2 11
// 18  5  4  3 12
// 17 16 15 14 13
//
// It can be verified that the sum of the numbers on the diagonals is 101.
// What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
//
// value of LR corner (n = 1, 2, 3...) = (2n-1)^2 + 2n
// value of LL corner = LR + 2n
// value of UL corner = LR + 4n
// value of UR corner = LR + 6n
// sum of all 4 = 16n^2 - 16n + 4 + 20n = 16n^2 + 4n + 4
// f(n) - f(n-1) = 16n^2 + 4n + 4, where f(n) = an^3 + bn^2 + cn + d

// f(n) - f(n-1) = a*(3n^2 - 3n + 1) + b*(2n - 1) + c = n^2(3a) + n(-3a + 2b) + (a - b + c)
// 16 = 3a
// 4 = 2b - 3a
// 4 = a - b + c
// a = 16/3
// b = 10
// c = 4 - 16/3 + 10 = 42/3 - 16/3 = 26/3
// f(n) = 16/3n^2 + 10n + 26/3

object Problem28 {
  def main(args: Array[String]) = {
    time {
      def f(n: Int) = 16*n*n + 4*n + 4   
      val result = (1 to 500).map(f(_)).sum + 1
      println("Problem28: " + result)
    }
  }
}