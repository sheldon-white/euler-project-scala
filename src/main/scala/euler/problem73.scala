package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
// If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
//
// 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
//
// It can be seen that there are 3 fractions between 1/3 and 1/2.
//
// How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?

object Problem73 {
  def gcd(m: Int, n: Int): Int = {
    val n1 = max(m, n)
    val n2 = min(m, n)
    (n1, n2) match {
      case (a, b) if (a % b == 0) => b
      case _ => gcd(n2, n1 % n2)
    }
  }

  def primeFractionsInRange(n: Int) = {
    val low = n % 3 match {
      case 0 => n / 3 + 1
      case _ => ceil(n.toDouble / 3).toInt
    }
    val high = n % 2 match {
      case 0 => n / 2 - 1
      case _ => floor(n.toDouble / 2).toInt
    }

    val relPrimes = for (i <- low to high; if gcd(n, i) == 1) yield (i, n)
    relPrimes
  }
  
  def main(args: Array[String]) = {
    time {
      val allFracs = (1 to 12000).map(primeFractionsInRange).flatten
      //println(allFracs)
      println("Problem 73: " + allFracs.size)
    }
  }
}