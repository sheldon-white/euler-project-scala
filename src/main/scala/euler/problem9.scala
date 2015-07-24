package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
object Problem9 {
  var solutions = for (a <- 1 to 1000; b <- 1 to 1000; c = 1000 - a - b;  if (a*a+b*b==c*c)) yield (a, b, c)
  
  def main(args: Array[String]) = {
    println("Problem 9: " + solutions)    
  }
}
