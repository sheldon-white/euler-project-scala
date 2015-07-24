package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._


// Find the sum of all the even-valued terms in the sequence which do not exceed four million.
object Problem2 {
  def main(args: Array[String]) = {
    lazy val fib: Stream[Int] = Stream.cons(0,Stream.cons(1, fib.zip(fib.tail).map(p => p._1 + p._2)))
    var sum = fib.take(100).filter { _ % 2 == 0} takeWhile { _ < 1000000 } reduceLeft {_ + _}
    println("Problem 2: " + sum)    
  }
}
