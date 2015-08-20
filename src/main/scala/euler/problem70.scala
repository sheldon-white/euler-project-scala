package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of
// positive numbers less than or equal to n which are relatively prime to n.
// For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
// The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
//
// Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
// Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.
object Problem70 {
  
  def main(args: Array[String]) = {
    
    def totient(n: Int) = {
      val factors = Prime.factors(n)
      val top = factors.map(_ - 1).foldLeft(1)((a, b) => a * b).toDouble
      val bottom = factors.foldLeft(1)((a, b) => a * b)
      (n, n * top / bottom toInt)
    }
    
    def isPermutation(t: Tuple2[Int, Int]) = {
      t._1.toString.toList.sorted == t._2.toString.toList.sorted
    }
    
    time {
      breakable {
        //println(totient(87109))
        val matches = ((for (i <- 2 until 10000000) yield(totient(i))).filter(isPermutation(_))).toMap
        //println(matches.minBy(_._2))
        println(matches)
        println("Problem 69: " + matches.minBy(t => t._1.toDouble / t._2)._1)
      }
    }
  }
}