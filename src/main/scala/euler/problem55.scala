package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
// Not all numbers produce palindromes so quickly. For example,
// 349 + 943 = 1292,
// 1292 + 2921 = 4213
// 4213 + 3124 = 7337
// That is, 349 took three iterations to arrive at a palindrome.
// Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome.
// A number that never forms a palindrome through the reverse and add process is called a Lychrel number.
// Due to the theoretical nature of these numbers, and for the purpose of this problem,
// we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number
// below ten-thousand, it will either (i) become a palindrome in less than fifty iterations,
// or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome.
// In fact, 10677 is the first number to be shown to require over fifty iterations before producing
// a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
// Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
// How many Lychrel numbers are there below ten-thousand?


object Problem55 {
  def isPalindromic(num: BigInt) = num.toString.reverse == num.toString 
  def next(num: BigInt) = num + BigInt(num.toString.reverse)
  def isLychrel(num: BigInt): Boolean = {
    var n = num
    for (i <- 1 until 50) {
      n = next(n)
      //println(i + ": " + n)
      if (isPalindromic(n)) {
        return false
      }
    }
    println(num + " is lychrel")
    return true
  }
  
  def main(args: Array[String]) = {
    time {
      //println(isLychrel(BigInt(2987)))
      println("Problem 55: " + (1 until 10000).filter(i => isLychrel(BigInt(i))).size)
    }
  }
}
