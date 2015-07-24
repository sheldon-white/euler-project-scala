package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._


// What is the largest prime factor of the number 600851475143 ?
object Problem3 {
  def main(args: Array[String]) = {
    var theNum = 317584931803L
    lazy val naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))
    var result = naturals.drop(1).dropWhile(n => {while(theNum % n == 0) {theNum /= n}; theNum > 1});
    println("Problem 3: " + result.take(1))   
  }
}
