package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

// Problem 6: Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
object Problem6 {
  def x = (1 to 100) reduceLeft { (a: Int, b:Int) => a + (b * b)}
  def y = (1 to 100) reduceLeft { (a: Int, b:Int) => a + b}
  def diff = y * y - x

	def main(args: Array[String]) = {
   println("Problem 6: " + diff)   
	}
}
