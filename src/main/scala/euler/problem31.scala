package euler

import scala.collection.mutable._
import scala.math._
import util.Time._

// In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
//
// 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
// It is possible to make £2 in the following way:
//
// 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
// How many different ways can £2 be made using any number of coins?

// The total is the sum of the counts where each successive coin is the largest coin used.
// also, we can cache counts for sub-values

// count(num, coins) = coins map {topCoin => countWithTop(num, coins, topCoin)} sum

object Problem31 {
  val coins = Vector(200, 100, 50, 20, 10, 5, 2, 1)
  
  def main(args: Array[String]) = {
    time {
      println("Problem 31: " + Partitions.partitionsFromSet(200, coins))
    }
  }
}
