package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._
import util.Time._

// Starting in the top left corner of a 2 x 2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
// How many such routes are there through a 20 x 20 grid?

object Problem15 {
	def factorial(number: Int) : BigInt = {
		def factorialWithAccumulator(accumulator: BigInt, number: Int) : BigInt = {
			if (number == 1) 
				return accumulator
			else
				//println(accumulator)
				factorialWithAccumulator(accumulator * number, number - 1)
			}
			factorialWithAccumulator(1, number)
	}

  def routesToPoint(number: Int) = {
    var fac1 = factorial(number)
    var fac2 = factorial(2 * number)
    fac2 / (fac1 * fac1)
  }
  
	def main(args: Array[String]) = {
		println("Problem 15: " + routesToPoint(20))  
	}
}
