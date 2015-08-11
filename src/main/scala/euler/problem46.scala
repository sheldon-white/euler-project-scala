package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._

// It was proposed by Christian Goldbach that every odd composite number can be written as
// the sum of a prime and twice a square.
// 9 = 7 + 2×12
// 15 = 7 + 2×22
// 21 = 3 + 2×32
// 25 = 7 + 2×32
// 27 = 19 + 2×22
// 33 = 31 + 2×12
// It turns out that the conjecture was false.
// What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

object Problem46 {
  
  def composites = (for (f1 <- 3 to 500 by 2; f2 <- f1 to 500 by 2; if isMatch(f1 * f2)) yield f1 * f2).sorted

  def isMatch(composite: Int): Boolean = {
    val maxRoot = sqrt(composite / 2).intValue()
    val fails = (1 to maxRoot) filter {i => Prime.isPrime(composite - 2 * i * i)}
    fails.length == 0
  }
  
  def main(args: Array[String]) = {
    println("Problem 46: " + composites(0))
  }
}
