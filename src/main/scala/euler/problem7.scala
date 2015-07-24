package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

// Problem 7: What is the 10001st prime number?
object Problem7 {
  var ps:Stream[Int] = Stream.cons(2,Stream.from(3).filter{ n => ps.takeWhile(p => p*p <= n).forall(n%_ !=0) })
  def apply(): Unit =
  {
    println("Problem 7: " + ps(10000))    
  }
}
