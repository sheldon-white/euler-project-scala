package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

//In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
//by only moving to the right and down, is indicated in bold red and is equal to 2427.
//
//Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
//a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down.
object Problem81 { 
  def readMatrix() = {
    val stream : InputStream = getClass.getResourceAsStream("p081_matrix.txt")
    val lines = scala.io.Source.fromInputStream(stream).getLines
    val numbers = lines.map(_.split(",").map(_.toInt).toVector).toVector
    //println(numbers)
    numbers
  }

  def convertToDiamond(matrix: Vector[Vector[Int]]) = {
    val diamond = ArrayBuffer[ArrayBuffer[Tuple2[Int, Int]]]()
    for (i <- 0 to 2 * matrix.size - 2) {
      val v = ArrayBuffer[Tuple2[Int, Int]]()
      var x = max(0, i - matrix.size + 1)
      var y = min(i, matrix.size - 1)
      while (y > -1 && x < matrix.size) {
        //println(x, y)
        v += Tuple2(matrix(y)(x), -1)
        x += 1
        y -= 1
      }
      diamond += v
    }
    diamond //.toVector
  }

  def generateMinPaths(triangle:ArrayBuffer[ArrayBuffer[Tuple2[Int, Int]]]) = {
    val squareSide = (triangle.length + 1) / 2
     for (row <- 0 until triangle.length) {
      for (col <- 0 until triangle(row).length) {
        var minPred = minPredecessor(triangle, squareSide, row, col)
        //println("minPred for",row, col, minPred)
        var minVal = triangle(row)(col)._1 + minPred
        //println("min for",row, col, minVal)
        triangle(row)(col) = Tuple2(triangle(row)(col)._1, minVal)
      }
    }
    triangle
  }
  
  def minPredecessor(triangle:ArrayBuffer[ArrayBuffer[Tuple2[Int, Int]]], squareSide: Int, row:Int, col:Int): Int = {
    (row, col) match {
      case (0, 0) => 0
      case (row, 0) if (row < squareSide) => triangle(row - 1)(0)._2
      case (row, col) if (row < squareSide && col == triangle(row).length - 1) => triangle(row - 1)(col - 1)._2
      case (row, col) if (row < squareSide) => min(triangle(row - 1)(col - 1)._2, triangle(row - 1)(col)._2)
      case _ => min(triangle(row - 1)(col)._2, triangle(row - 1)(col + 1)._2)
    }
  }

  def main(args: Array[String]) = {
    time {
      val matrix = readMatrix
//      val matrix = Vector(
//          Vector(131,673,234,103,18),
//          Vector(201,96,342,965,150),
//          Vector(630,803,746,422,111),
//          Vector(537,699,497,121,956),
//          Vector(805,732,524,37,331))
      val diamond = convertToDiamond(matrix)
      println(diamond)
      val minPaths = generateMinPaths(diamond)
      println("Problem 81: " + minPaths(minPaths.length - 1).minBy(_._2)._2)
    }
  }
}