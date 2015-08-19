package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// By starting at the top of the triangle below and moving to adjacent numbers on the row below,
// the maximum total from top to bottom is 23.

//3
//7 4
//2 4 6
//8 5 9 3
//
//That is, 3 + 7 + 4 + 9 = 23.
//
//Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.
//
//NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, as there are 299 altogether! If you could check one trillion (1012) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)

object Problem67 {
  def readTriangle() = {
    val stream : InputStream = getClass.getResourceAsStream("p067_triangle.txt")
    val lines = scala.io.Source.fromInputStream(stream).getLines
    val numbers = lines.map(_.split(" ").map(_.toInt).toVector).toVector
    //println(numbers)
    numbers
  }

  def generateMaxPaths(triangle:Vector[Vector[Int]]): Vector[Vector[Int]] = {
    var maxPaths = ArrayBuffer[Vector[Int]]()
    for (i <- 0 until triangle.length) {
      val triangleRow = triangle(i)
      var maxPathsRow = ArrayBuffer[Int]()
      for (j <- 0 until triangleRow.length) {
        var maxVal = triangleRow(j) + maxPredecessor(maxPaths, i, j)
        maxPathsRow += maxVal
        //print(maxVal + " ")
      }
      maxPaths += maxPathsRow.toVector
    }
    maxPaths toVector
  }
  
//  def maxPredecessor(triangle:ArrayBuffer[Vector[Int]], i:Int, j:Int): Int = {
//    if (i == 0) {
//      return 0
//    } else if (j == 0) {
//      return triangle(i - 1)(j)
//    } else if (j == triangle(i - 1).length) {
//        return triangle(i - 1)(j - 1)
//    } else {
//      return max(triangle(i - 1)(j - 1), triangle(i - 1)(j))
//    }
//  }
  
  def maxPredecessor(triangle:ArrayBuffer[Vector[Int]], i:Int, j:Int): Int = {
    (i, j) match {
      case (0, j) => 0
      case (i, 0) => triangle(i - 1)(j)
      case (i, j) if (j == triangle(i - 1).length) => triangle(i - 1)(j - 1)
      case _ => max(triangle(i - 1)(j - 1), triangle(i - 1)(j))
    }
  }
  
//  def getMaxPath(triangle:ArrayBuffer[ArrayBuffer[Int]]): List[Int] = {
//    var maxPath = List[Int]()
//    maxPath
//  }

  def main(args: Array[String]) = {
    time {
      breakable {
        val triangle = readTriangle
        val maxPaths = generateMaxPaths(triangle)
        println("Problem 67: " + maxPaths(maxPaths.length - 1).max)
      }
    }
  }
}