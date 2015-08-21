package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
//If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
//1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
//It can be seen that 2/5 is the fraction immediately to the left of 3/7.
//By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.

//
// I don't agree with the answer I've seen on solution pages: 428570
//
object Problem71 {
  def gcd(m: Int, n: Int): Int = {
    val n1 = max(m, n)
    val n2 = min(m, n)
    (n1, n2) match {
      case (a, b) if (a % b == 0) => b
      case _ => gcd(n2, n1 % n2)
    }
  }
  
  def main(args: Array[String]) = {
    val threeSeventh = 3.0/7
    val vals = HashMap[Double, Tuple2[Int, Int]]()
    for (m <- 428569 to 428578; n <- 999998 to 1000000) {
      val a = m.toDouble / n
      if (a < threeSeventh) {
        vals(a) = (m, n)
      }
    }
    
    val minVal = vals.maxBy(_._1)
    println(vals.maxBy(_._1))
    time {
      breakable {
        println("Problem 71: " + minVal._2._1)
      }
    }
  }
}