package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// It is well known that if the square root of a natural number is not an integer, then it is irrational.
// The decimal expansion of such square roots is infinite without any repeating pattern at all.
//
// The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.
//
// For the first one hundred natural numbers, find the total of the digital sums of
// the first one hundred decimal digits for all the irrational square roots.
// 100 digits = 1.414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641572
// val r2 = "1.414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641572"
// r2.toList.filterNot(_ == '.').map(_ - '0').sum == 475

object Problem80 { 
   def bdSqrt(d: BigDecimal) = {
    var temp = d / 2
    for (i <- 1 to 20) {
      val next = d / temp
      temp = (next + temp) / 2
    }   
    temp
  }
  
  def digitSum(d: BigDecimal) = {
    d.toString.slice(0, 101).toVector.filterNot(_ == '.').map(_ - '0').sum
  }

  def isSquare(n: Int) = {
    sqrt(n) == floor(sqrt(n))
  }
  def main(args: Array[String]) = {
    time {
      val context = new java.math.MathContext(110)
      val answer = (1 to 100).filterNot(isSquare).map(BigDecimal(_, context)).map(bdSqrt(_)).map(digitSum).sum
      println("Problem 80: " + answer)
    }
  }
}