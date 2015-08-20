package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.
// (n1, n2, n3), (n4, n3, n5), (n6, n5, n2)
// Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.
//
// It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
//
// Total Solution Set
// 9   4,2,3; 5,3,1; 6,1,2
// 9   4,3,2; 6,2,1; 5,1,3
// 10  2,3,5; 4,5,1; 6,1,3
// 10  2,5,3; 6,3,1; 4,1,5
// 11  1,4,6; 3,6,2; 5,2,4
// 11  1,6,4; 5,4,2; 3,2,6
// 12  1,5,6; 2,6,4; 3,4,5
// 12  1,6,5; 3,5,4; 2,4,6
// By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.
//
// Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings.
// What is the maximum 16-digit string for a "magic" 5-gon ring?
// (n0, n1, n2), (n3, n2, n4), (n5, n4, n6), (n7, n6, n8), (n9, n8, n1)

object Problem68 {
  def isMatch(v: Vector[Int]): Boolean = {
    val s = v(0) + v(1) + v(2)
    s == v(3) + v(2) + v(4) && s == v(5) + v(4) + v(6) && s == v(7) + v(6) + v(8) && s == v(9) + v(8) + v(1)
  }
  
  def mkStringFromLowestExternal(v: Vector[Int]): String = {
    val minOuter = Vector(0, 3, 6, 9, 12) map {v} min
    val minIndex = v indexOf minOuter
    val folded = v.slice(minIndex, v.length) ++ v.slice(0, minIndex)
    folded mkString("")
  }
  
  def main(args: Array[String]) = {
    time {
      breakable {
        val maxValue = (1 to 10).toVector.permutations.filter(isMatch).
          map(v => Vector(v(0),v(1),v(2),v(3),v(2),v(4),v(5),v(4),v(6),v(7),v(6),v(8),v(9),v(8),v(1))).
          map(mkStringFromLowestExternal).filter(_.length == 16).map(_.toLong).max
        println("Problem 68: " + maxValue)
      }
    }
  }
}