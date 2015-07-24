package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

// What is the first triangular number with 500+ divisors?
object Problem12 {
  def factorCount(n: Int): Int = {
    (for (i <- 1 to 1 + sqrt(n).toInt; if n % i == 0) yield (i)).toList.length + 1
  }
    
  def main(args: Array[String]) = {
    val triangles = Stream.from(1).map(n => n * (n + 1) / 2)
    //println(factorCount(124750))
    //println("Problem 12: " + triangles.take(500).toList.map {i => (i, factorCount(i)) })    
    println("Problem 12: " + triangles.filter({i => factorCount(i) >= 500}).head)
  }
}
