package euler

import scala.collection.mutable._
import scala.math._
import util.Prime

// The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719,
// are themselves prime.
// There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
// How many circular primes are there below one million?
object Problem35 {
  
  def transformString(s:String):List[String] = 
    for(i <- s.length until 0 by - 1 toList) yield s.drop(i)+ s.take(i)
  
  def isCircular(num: Int): Boolean = {
    val permutations = transformString(num.toString())
    for (permutation <- permutations) {
      if (!Prime.isPrime(permutation.toInt)) {
        return false
      }
    }
    return true
  }
  
  def main(args: Array[String]) = {
    var total = (2 to 1000000).filter(n => isCircular(n)).size
    println("Problem 34: " + total)    
  }
}
