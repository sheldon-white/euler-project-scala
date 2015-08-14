package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import util.Time._

// Take the number 192 and multiply it by each of 1, 2, and 3:
//
// 192 × 1 = 192
// 192 × 2 = 384
// 192 × 3 = 576
// By concatenating each product we get the 1 to 9 pandigital, 192384576.
// We will call 192384576 the concatenated product of 192 and (1,2,3)
// The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645,
// which is the concatenated product of 9 and (1,2,3,4,5).
// What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

// possible solutions:
// ABC * (1,2,3) (A < 4)
// AB * (1, 2, 3, 4) (A = 1, 2, 3)
// ABCD * (1, 2) (A > 4)
object Problem38 {
  val target = List(1,2,3,4,5,6,7,8,9)
  def numberIsPandigital(number: Int): Boolean = {
    val sorted = number.toString.toList.sorted.map(n => (n - '0').toInt)
    return sorted == target
  }
  
  var allSolutions = ListBuffer[Int]()
  
  def main(args: Array[String]) = {
    time {
      def solutions0 = List(918273645)
      def case1Combos = for (a <- 1 to 3; b <- 1 to 9; c <- 1 to 9) yield 100*a + 10*b + c 
      def solutions1 = case1Combos map {n => n * 1002003} filter {n => numberIsPandigital(n)}
      def case2Combos = for (a <- 1 to 3; b <- 1 to 9) yield 10*a + b
      def solutions2 = case2Combos map {n => n * 10203004} filter {n => numberIsPandigital(n)}
      def case3Combos = for (a <- 1 to 9; b <- 1 to 9; c <- 1 to 9; d <- 1 to 9) yield 1000*a + 100*b + 10*c + d
      def solutions3 = case3Combos map {n => n * 100002} filter {n => numberIsPandigital(n)}
      def solution = List(solutions0, solutions1, solutions2, solutions3).flatten.sorted.last
      println("Problem 38: " + solution)
    }
  }
}
