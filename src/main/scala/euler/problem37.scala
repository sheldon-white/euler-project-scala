package euler

import scala.collection.mutable._
import scala.math._
import util.Prime

// The number 3797 has an interesting property. Being prime itself, it is possible to continuously
// remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7.
// Similarly we can work from right to left: 3797, 379, 37, and 3.
// Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
// NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

// start with 1, 3, 7, 9
// build out truncatable primes until we find em all
object Problem37 {
  
  object CheckMode extends Enumeration {
    type CheckMode = Value
    val CHECK_FROM_LEFT, CHECK_FROM_RIGHT = Value
  }
  import CheckMode._

  var allSolutions = HashSet[Long]()
  
  def findTruncatables(num: Long): Unit = {
    // see if num is truncatable in the specified direction
    List(1, 3, 7, 9) foreach { i =>
      // case 1
      val c1 = 10 * num + i
        println("checking " + c1)
      if (checkKids(c1, CHECK_FROM_LEFT) && checkKids(c1, CHECK_FROM_RIGHT)) {
        println(c1 + " good")
        allSolutions.add(c1)
        findTruncatables(c1)
      }
      // case 2
      val c2 = (i + num.toString).toLong
        println("checking " + c2)
      if (checkKids(c2, CHECK_FROM_LEFT) && checkKids(c2, CHECK_FROM_RIGHT)) {
        println(c2 + " good")
        allSolutions.add(c2)
        findTruncatables(c2)       
      }
    }
  }
  
  def checkKids(num: Long, direction: CheckMode): Boolean = {
    direction match {
      case CHECK_FROM_LEFT => {
        // remove digits from the left side, check each result
        var n = num
        while (n > 0) {
          if (!Prime.isPrime(n)) {
            return false
          }
          n /= 10
        }
        return true
      }
      case CHECK_FROM_RIGHT => {
        // remove digits from the right side, check each result
        var n = num
        while (n > 0) {
          if (!Prime.isPrime(n)) {
            return false
          }
          if (n < 10) {
            return true
          }
          n = (n.toString.drop(1)).toLong
        }
        return true
      }
    }
  }
  
  def main(args: Array[String]) = {
//    println(Prime.isPrime(1))
//    List(1, 2, 3, 5, 7, 9) foreach { findTruncatables(_) }
//    println(allSolutions)
//    allSolutions.clear()
    for (i <- 11 to 1000000) {
      if (Prime.isPrime(i) && checkKids(i, CHECK_FROM_RIGHT) && checkKids(i, CHECK_FROM_LEFT)) {
        allSolutions.add(i)
      }
    }
    println(allSolutions)
    println("Problem 37: " + allSolutions.sum)
  }
}
