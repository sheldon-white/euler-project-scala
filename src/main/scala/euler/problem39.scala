package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import util.Time._

// If p is the perimeter of a right angle triangle with integral length sides, {a,b,c},
// there are exactly three solutions for p = 120.
// {20,48,52}, {24,45,51}, {30,40,50}
// For which value of p ≤ 1000, is the number of solutions maximised?
//
// let's just search for solutions and see what their sides add up to
object Problem39 {
  var counts = HashMap[Int, Int]()
  def findTriples(maxPerimeter: Int) = {
    var p2 = maxPerimeter/2
    for (x <- 2 to p2; y <- x+1 to p2) {
      var z = y+1
      while (z <= p2) {
        while (z*z < x*x + y*y) {
          z += 1
        }

        if (z*z == x*x + y*y && x+y+z <= maxPerimeter) {
          val sum = x + y + z
          if (counts.contains(sum)) {
            counts(sum) = counts(sum) + 1
          } else {
            counts(sum) = 1
          }
        }
        z += 1
      }
    }
  }
  
  def main(args: Array[String]) = {
    time {
      findTriples(1000)
      val solution = counts.maxBy(_._2)._1
      println(counts(solution))
      println("Problem 39: " + solution)
    }
  }
}
