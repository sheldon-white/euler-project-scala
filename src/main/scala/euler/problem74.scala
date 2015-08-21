package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:
//
// 1! + 4! + 5! = 1 + 24 + 120 = 145
//
// Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169; it turns out that there are only three such loops that exist:
//
// 169 → 363601 → 1454 → 169
// 871 → 45361 → 871
// 872 → 45362 → 872
//
// It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,
//
// 69 → 363600 → 1454 → 169 → 363601 (→ 1454)
// 78 → 45360 → 871 → 45361 (→ 871)
// 540 → 145 (→ 145)
//
// Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.
//
// How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?
object Problem74 {
  val factorials = List(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
  
  def factorialSum(num: Int): Int = {
    var n = num
    var sum = 0
    while (n > 0) {
      val digit = n % 10
      sum += factorials(digit)
      n = n / 10
    }
    sum
  }
  
  def chainLength(num: Int) = {
     val seen = LinkedHashSet[Int]()
     var n = num
     while (!seen.contains(n)) {
       seen += n
       n = factorialSum(n)
     }
     seen.size
  }
  
  def main(args: Array[String]) = {
    time {
      val matches = (1 to 1000000).map(chainLength).filter(_ == 60)
      println("Problem 74: " + matches.size)
    }
  }
}