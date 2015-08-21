package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

//Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
//
//If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
//
//1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
//
//It can be seen that there are 21 elements in this set.
//
//How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?

// Seems like for each denominator, the number of reduced proper fractions it contributes is just φ(n), the number of
// numbers < n that are relatively prime to n.
object Problem72 {
  def totient(n: Int) = {
    val factors = Prime.factors(n)
    val top = factors.map(_ - 1).foldLeft(1)((a, b) => a * b).toDouble
    val bottom = factors.foldLeft(1)((a, b) => a * b)
    n * top / bottom toLong
  }

  def main(args: Array[String]) = {
    
    time {
      breakable {
        val count = (1 to 1000000) map {totient}
        println("Problem 72: " + (count.sum - 1))
      }
    }
  }
}