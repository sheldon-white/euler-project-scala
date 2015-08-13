package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits,
// but in a different order.
// Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

object Problem52 {
  def allArePermutations(numbers: Vector[Int]): Boolean = {
    def sortedDigits(number: Int) = number.toString.toList.sorted.toVector
    def firstSet = sortedDigits(numbers(0))
    numbers drop(1) forall {n => sortedDigits(n) == firstSet}
  }

  def isMatch(num: Int) = {
   allArePermutations(Vector(num, 2*num, 3*num, 4*num, 5*num, 6*num))   // allArePermutations(Vector(num, 2*num))
  }
  
  def main(args: Array[String]) = {
    time {
      println("Problem 52: " + ((2 to 10000000) find {isMatch}))
    }
  }
}
