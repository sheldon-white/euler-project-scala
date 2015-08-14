package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._


// There are exactly ten ways of selecting three from five, 12345:
// 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
// In combinatorics, we use the notation, 5C3 = 10.
// In general, nCr = n!/r!(n−r)! , where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
// It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
// How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?

object Problem53 {
  def nChooseR(n: Int, r: Int): BigInt = {
    def f2(n: Int, result: BigInt): BigInt = if (n==0) result else f2(n-1, n*result)
    def f(n: Int):BigInt = f2(n, 1)
    
    (n-r) match {
      case 0 => 1
      case _ => f(n) / (f(r) * f(n-r))
    }
  }
  
  def findMatches = for (n <- 1 to 100; r <- 1 to n; if nChooseR(n, r) >= 1000000) yield (n, r)

  def main(args: Array[String]) = {
    time {
      println("Problem 53: " + findMatches.size)
    }
  }
}
