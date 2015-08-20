package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of numbers
// less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8,
// are all less than nine and relatively prime to nine, φ(9)=6.
//
// n Relatively Prime  φ(n)  n/φ(n)
// 2 1 1 2
// 3 1,2 2 1.5
// 4 1,3 2 2
// 5 1,2,3,4 4 1.25
// 6 1,5 2 3
// 7 1,2,3,4,5,6 6 1.1666...
// 8 1,3,5,7 4 2
// 9 1,2,4,5,7,8 6 1.5
// 10  1,3,7,9 4 2.5
// It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.
//
// Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

// φ(n) = n * PI{p|n} (1 - 1/p), so n / φ(n) = 1 / (PI{p|n} (1 - 1/p))

object Problem69 {
  
  def main(args: Array[String]) = {
    
    def calculateValue(n: Int): Double = {
      val factors = Prime.factors(n)
      val bottom = factors.map(_ - 1).foldLeft(1)((a, b) => a * b).toDouble
      val top = factors.foldLeft(1)((a, b) => a * b)
      top / bottom
    }
    
    time {
      breakable {
        val values: Map[Int, Double] = (for (i <- 2 to 1000000) yield(i, calculateValue(i))) (breakOut)
        println("Problem 69: " + values.maxBy(_._2)._1)
      }
    }
  }
}