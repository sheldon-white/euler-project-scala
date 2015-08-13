package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._
import BigInt._
import util.Time._

// Find the sum of the digits in the number 100!
object Problem20 {
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
    time {
      println("Problem 20: " + factorial(100).toString.toList.foldLeft(0)(_ + _ - '0'))
    }
  }
}
