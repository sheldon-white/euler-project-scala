package euler

import scala.collection.mutable._
import scala.math._
import util.Prime

// An irrational decimal fraction is created by concatenating the positive integers:
// 0.123456789101112131415161718192021...
// It can be seen that the 12th digit of the fractional part is 1.
// If dn represents the nth digit of the fractional part, find the value of the following expression.
// d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

// find starting index for the target offset. (offset-startingIndex) / magnitude = the n-digit number
// then find the correct digit
object Problem40 {
  val champernownes = (for (i <- 0 to 200000) yield i.toString()).mkString("")
 
  def main(args: Array[String]) = {
    val result = List(1, 10, 100, 1000, 10000, 100000, 1000000).map(i => (champernownes(i) - '0').toInt).foldLeft(1)(_ * _)
    println("Problem 40: " + result)
  }
}
