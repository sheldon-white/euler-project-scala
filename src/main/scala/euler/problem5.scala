package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

// Problem 5: What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
object Problem5 {
  var product: Long = 1
  var maxVal = 20
  for (x <- 2 to maxVal) {
    (product % x) match {
      case 0 =>
      case _ => {
        //println(product + " not div by " + x);
        var pow = 1; while (pow <= maxVal) {pow *= x}; product *= pow/x;
      }
    }
  }

	def main(args: Array[String]) = {
    println("Problem 5: " + product)    
	}
}
