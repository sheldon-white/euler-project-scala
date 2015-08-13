package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._
import BigInt._
import util.Time._

//By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23,.
//
//3
//7 4
//2 4 6
//8 5 9 3
//
//That is, 3 + 7 + 4 + 9 = 23.
//
//Find the maximum total from top to bottom of the triangle below:
//
//75,
//95, 64,
//17, 47, 82,
//18, 35, 87, 10,
//20,  4, 82, 47, 65,
//19,  1, 23, 75,  3, 34,
//88,  2, 77, 73,  7, 63, 67,
//99, 65,  4, 28,  6, 16, 70, 92,
//41, 41, 26, 56, 83, 40, 80, 70, 33,
//41, 48, 72, 33, 47, 32, 37, 16, 94, 29,
//53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14,
//70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57,
//91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48,
//63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31,
//04, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23,
//
//NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
// However, Problem 67 is the same challenge with a triangle containing one-hundred rows;
// it cannot be solved by brute force, and requires a clever method! ;o)
object Problem18 {

  def generateMaxPaths(triangle:Array[Array[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
    var maxPaths = ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 0 until triangle.length) {
      val triangleRow = triangle(i)
      var maxPathsRow = ArrayBuffer[Int]()
      maxPaths += maxPathsRow
      for (j <- 0 until triangleRow.length) {
        var maxVal = triangleRow(j) + maxPredecessor(maxPaths, i, j)
        maxPathsRow += maxVal
        //print(maxVal + " ")
      }
      //println()
    }
    maxPaths
  }
  
  def maxPredecessor(triangle:ArrayBuffer[ArrayBuffer[Int]], i:Int, j:Int): Int = {
    if (i == 0) {
      return 0
    } else if (j == 0) {
      return triangle(i - 1)(j)
    } else if (j == triangle(i - 1).length) {
        return triangle(i - 1)(j - 1)
    } else {
      return max(triangle(i - 1)(j - 1), triangle(i - 1)(j))
    }
  }
  
  def getMaxPath(triangle:ArrayBuffer[ArrayBuffer[Int]]): List[Int] = {
    var maxPath = List[Int]()
    maxPath
  }
  
	def main(args: Array[String]) = {
    time {
      val triangle = Array(
      Array(75),
      Array(95, 64),
      Array(17, 47, 82),
      Array(18, 35, 87, 10),
      Array(20,  4, 82, 47, 65),
      Array(19,  1, 23, 75,  3, 34),
      Array(88,  2, 77, 73,  7, 63, 67),
      Array(99, 65,  4, 28,  6, 16, 70, 92),
      Array(41, 41, 26, 56, 83, 40, 80, 70, 33),
      Array(41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
      Array(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
      Array(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
      Array(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
      Array(63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
      Array( 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23)
      )
      // generate triangle of maximum path totals
      val maxPaths = generateMaxPaths(triangle)
      println("Problem 18: " + maxPaths(maxPaths.length - 1).max)
    }
  }
}
