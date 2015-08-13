package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._

// Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

// Triangle    Tn=n(n+1)/2   1, 3, 6, 10, 15, ...
// Pentagonal    Pn=n(3n−1)/2    1, 5, 12, 22, 35, ...
// Hexagonal   Hn=n(2n−1)    1, 6, 15, 28, 45, ...
// It can be verified that T285 = P165 = H143 = 40755.
// Find the next triangle number that is also pentagonal and hexagonal.
object Problem45 {
  def isPentagonal(num: Long) = {
    val root = (1 + sqrt(24 * num + 1)) / 6
    floor(root) == root
  }
  
  def isTriangular(num: Long) = {
    val root = (1 + sqrt(8 * num + 1)) / 2
    floor(root) == root
  }
  
  def hexagon(num: Long): Long = {
    num * (2 * num - 1)
  }

  def isMatch(i: Long): Boolean = {
    val h = hexagon(i)
    isTriangular(h) && isPentagonal(h)
  }

  def findMatches(limit: Int) = {
    for (i <- 2 to limit; if isMatch(i)) yield hexagon(i)
  }
  
  
  def main(args: Array[String]) = {
    val matches = findMatches(100000).sorted
    println(matches)

    println("Problem 45: " + matches(1))
  }
}