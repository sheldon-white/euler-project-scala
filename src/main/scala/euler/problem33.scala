package euler

import scala.collection.mutable._
import scala.math._
import util.Time._

// The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may
// incorrectly believe that 49/98 = 4/8, which is correct, is obtained by canceling the 9s.
// We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
// There are exactly four non-trivial examples of this type of fraction, less than one in value,
// and containing two digits in the numerator and denominator.
// 
// If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

object Problem33 {
  var permutations = for (a <- 1 to 9; b <- 1 to 9; c <- 1 to 9) yield (a, b, c)
  var solutions = ListBuffer[Tuple2[Int, Int]]()
  
  def checkPermutation(combination: Tuple3[Int, Int, Int]) = {
    val a = combination._1
    val b = combination._2
    val c = combination._3
    // case 1: ab/bc = a/c (a < b)
    if (a < b && b != 0) {
      val t1 = (10*a + b).toFloat
      val b1 = (10*b + c).toFloat
      val t2 = a.toFloat
      val b2 = c.toFloat
      val q1 = t1 / b1
      val q2 = t2 / b2
      if (q1 == q2) {
        println("case 1")
        solutions += Tuple2(t1.toInt, b1.toInt)
      }
    }
    // case 2: ab/ca = b/c (a < c)
    if (a < c && c != 0) {
      val t1 = (10*a + b).toFloat
      val b1 = (10*c + a).toFloat
      val t2 = b.toFloat
      val b2 = c.toFloat
      val q1 = t1 / b1
      val q2 = t2 / b2
      if (q1 == q2) {
        println("case 2")
        solutions += Tuple2(t1.toInt, b1.toInt)
      }
    }
  }
  
  def main(args: Array[String]) = {
    time {
      permutations foreach checkPermutation
      val numerator = solutions.foldLeft(1) {_ * _._1}
      val denominator = solutions.foldLeft(1) {_ * _._2}
      solutions foreach println
      println("Problem 33: " + denominator / numerator)
    }
  }
}
