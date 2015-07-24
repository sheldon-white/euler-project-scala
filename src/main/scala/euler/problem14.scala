package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._



// n -> n/2 (n is even)
// n -> 3n + 1 (n is odd)
// Which starting number, under one million, produces the longest chain?
object Problem14 {
  def nextVal(i: Long): Long =
  {
    if (i == 1)
    {
      return 0;
    }
    (i % 2) match
    {
      case 0 => return i / 2
      case 1 => return 3 * i + 1
    }
  }
  
  def sequenceLen(i: Int): Int =
  {
    lazy val naturals: Stream[Long] = Stream.cons(i, naturals.map(nextVal(_)))
    return naturals.takeWhile(_ != 0).toList.length
  }

  def main(args: Array[String]) = {
    var maxLen = 0
    var maxIdx = 0
    for (i <- 1 to 1000000)
    {
      var len = sequenceLen(i)
      if (len > maxLen)
      {
        maxLen = len
        maxIdx = i
      }
    }
    println("Problem 14: " + maxIdx + ", " + maxLen)  
  }
}
