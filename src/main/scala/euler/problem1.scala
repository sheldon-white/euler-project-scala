package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

// Add all the natural numbers below one thousand that are multiples of 3 or 5.
object Problem1 {
  def main(args: Array[String]) = {
    var sum = (1 to 1000) filter {a => a % 3 == 0 || a % 5 == 0} reduceLeft {_ + _}
    println("Problem 1: " + sum)    
  }
}
