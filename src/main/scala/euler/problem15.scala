package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

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

  def main(args: Array[String]) = {
    val fac20 = factorial(20)
    val fac40 = factorial(40)
    println("Problem 15: " + fac40 / (fac20 * fac20))  
  }
}
