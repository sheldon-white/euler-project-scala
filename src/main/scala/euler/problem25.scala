package euler

import scala.collection.mutable._
import scala.math._
import BigInt._
import java.io._
import util.Time._

// F(1) = 1, F(2) = 1, F(3) = 2 etc.
// What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
object Problem25 {
	val fibs: Stream[BigInt] = BigInt(1) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  def main(args: Array[String]) = {
    time {
      var smallFibs = fibs.takeWhile({_.toString.toList.length < 1000}).toList
      println(smallFibs.length)
    }
  }
}