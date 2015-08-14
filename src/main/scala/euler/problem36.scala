package euler

import scala.collection.mutable._
import scala.math._
import util.Time._

// The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
// Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
// (Please note that the palindromic number, in either base, may not include leading zeros.)

object Problem36 {
  def isDoublePalindrome(n: Int) = {
    var s = n.toString
    var b = n.toBinaryString
    s == s.reverse && b == b.reverse
  }
  
  def main(args: Array[String]) = {
    time {
      val total: Int = (1 until 1000000).filter(isDoublePalindrome).sum
      println("Problem 34: " + total)
    }
  }
}
