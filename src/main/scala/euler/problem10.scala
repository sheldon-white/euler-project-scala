package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._
import util.Prime

// Problem 10: Sum of all primes below 2 million?
object Problem10 {
  
  def main(args: Array[String]) = {
    var smallPrimeSum = (for (i <- 2 to 2000000; if Prime.isPrime(i)) yield(i)).foldLeft(0) (_ + _)
    println("Problem 10: " + smallPrimeSum)
  }
}
