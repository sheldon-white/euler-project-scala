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
  val coins = List(200, 100, 50, 20, 10, 5, 2, 1)
  //val coins = List(5, 2, 1)

  // key looks like "100-50": size-largestCoin
  var subCounts: Map[String, Int] = HashMap[String, Int]()
  
  def partitions(size: Int, coins: List[Int]): Int = {
    val key = size + "-" + coins(0)
    println("partitioning " + size + " with " + coins)
    if (subCounts.contains(key)) {
      return subCounts(key)
    } else {
      var count = 0
      var coinsSlice = coins
      for (coin <- coins) {
        if (size > coin) {
          count += partitions(size - coin, coinsSlice)
        } else if (size == coin) {
          count += 1
        }
        coinsSlice = coinsSlice.drop(1)
      }
      subCounts(key) = count
      //println(key + " = " + count)
      return count
    } 
  }
  
  def main(args: Array[String]) = {
    time {
      println("Problem 31: " + partitions(200, coins))
    }
  }
}
