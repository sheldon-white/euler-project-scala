package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053).
// In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
// Find the smallest cube for which exactly five permutations of its digits are cube.

object Problem62 {
  val stringCubes = HashMap[String, HashSet[Long]]()

  def printAnswer(cStr: String) = {
    val cubes = stringCubes(cStr)
    println("Problem 62: " + cubes.min)
  }
  
  def main(args: Array[String]) = {
    time {
      breakable {
        for (n <- 5L to 1000000) {
          val c = n*n*n
          val cStr = c.toString.toList.sorted.mkString("")         
          stringCubes.get(cStr) match {
            case None => stringCubes(cStr) = HashSet[Long](c)
            case h if h.get.size == 4 => {stringCubes(cStr) = stringCubes(cStr) += c; printAnswer(cStr); break}
            case _ => stringCubes(cStr) = stringCubes(cStr) += c
          }
        }
      }
    }
  }
}