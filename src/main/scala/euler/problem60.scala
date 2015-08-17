package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import util.FixedSizeFifo._
import scala.util.control.Breaks._

// The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order
// the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime.
// The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
// Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

object Problem60 {
  val maxPrime = 10000
  val smallPrimes = Prime.primes.takeWhile(_ < maxPrime) drop(3) toVector
  val splitMod3 = smallPrimes partition {p => p % 3 == 1}
  val primes1 = 3L +: splitMod3._1
  val primes2 = 3L +: splitMod3._2
  def groupsOf5(p: Vector[Long]) = p combinations(5) map {new CandidateSet(_)}
//  def groupsOf5(p: Vector[Long]) = {
//    val s = p.size
//    for (a <- 0 until s; b <- a+1 until s; c <- b+1 until s; d <- c+1 until s; e <- d+1 until s)
//      yield (p(a),p(b),p(c),p(d),p(e))
//  }
  
  //def pairs(p: Vector[Long]) = (p.combinations(2) ++ p.reverse.combinations(2)).toVector.map(_.mkString("").toLong)

  class CandidateSet(var primes: Vector[Long]) {
    val combos = (primes.combinations(2) ++ primes.reverse.combinations(2)).toVector.map(_.mkString("").toLong)
    def matchesKnownComposite(allComposites: Set[Long]) = !combos.find(allComposites.contains(_)).isEmpty
    def allPrime = combos forall {Prime.isPrime}
  }

  val knownComposites = HashSet[Long]()

  def isMatch(g: CandidateSet): Boolean = {
    if (g.matchesKnownComposite(knownComposites)) {
      return false
    }
    for (c <- g.combos) {
      if (!Prime.isPrime(c)) {
        knownComposites += c
        //println(c, "composite")
        return false
      }
    }
    return true
  }

  def main(args: Array[String]) = {
    time {
      breakable {
        for (group <- Vector(primes1, primes2)) {
          for (g <- groupsOf5(group)) {
            //println(g.combos)
            if (isMatch(g)) {
              println(g.combos)
              println(g.primes, "!!!!!!!!!!!!!!!!!")
              println(g.primes.sum)
              for (p <- g.combos) {
                if (Prime.isPrime(p)) {
                  println(p, "prime")
                } else {
                  println(p, "not prime")
                }
              }
              break
            }
          }
        }
      }
    }
  }
}
