package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._

// The first two consecutive numbers to have two distinct prime factors are:
// 14 = 2 × 7, 15 = 3 × 5
// The first three consecutive numbers to have three distinct prime factors are:
// 644 = 2² × 7 × 23
// 645 = 3 × 5 × 43
// 646 = 2 × 17 × 19.
// Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?

object Problem47 {  
  def findMatch(num: Int): Boolean = {
    (num to num + 3).map(Prime.factors(_).size) == List(4, 4, 4, 4)
  }
  
  def main(args: Array[String]) = {
    val matches = (2 to 1000000) find {findMatch}
    println("Problem 47: " + matches.get)
  }
}
