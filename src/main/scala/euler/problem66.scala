package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// Consider quadratic Diophantine equations of the form:
// x2 – Dy2 = 1
//
// For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.
// It can be assumed that there are no solutions in positive integers when D is square.
// By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
//
// 32 – 2×22 = 1
// 22 – 3×12 = 1
// 92 – 5×42 = 1
// 52 – 6×22 = 1
// 82 – 7×32 = 1
//
// Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
// Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
object Problem66 {
  def findCycle(s: Int): Vector[BigInt] = {
    var m_n = 0L
    var d_n = 1L
    val a_0 = sqrt(s).toLong
    var a_n = a_0
    var done = false
    val as = new ArrayBuffer[BigInt]()
    as += a_0
    
    while (!done) {
      val m_next = (d_n * a_n - m_n).toLong
      val d_next = ((s - m_next * m_next) / d_n).toLong
      if (d_next != 0) {
        val a_next = (a_0 + m_next) / d_next
        a_n = a_next
        d_n = d_next
        m_n = m_next
        as += a_n
        if (a_n == 2 * a_0) {
          done = true
        }
      } else {
        done = true
      }
    }
    as toVector
  }

  def calculatePartial(num: Int, seq: Vector[BigInt]): Tuple2[BigInt, BigInt] = {
    var b = BigInt(1)
    var rev = seq.reverse.drop(1)
    var a = rev(0)
    rev = rev.drop(1)
    for (n <- rev) {
      val aNext = n * a + b
      val bNext = a
      a = aNext
      b = bNext
      if (a*a - num * b * b == 1) {
        return (a, b)
      }
    }
    (a, b)
  }
  
  def isSquare(num: Long) = {
     val s = sqrt(num)
     floor(s) == s
  }
  
  def main(args: Array[String]) = {
    time {
      breakable {
        val xValues = new HashMap[Int, BigInt]()
        for (num <- 2 to 1000) {
          // FIXME skip squares
          if (!isSquare(num)) {
            var seq = findCycle(num)
            var partial = calculatePartial(num, seq)
            var x2 = partial._1 * partial._1
            var y2 = partial._2 * partial._2
            if (x2 - num * y2 == -1) {
              var extended = seq ++ (seq drop(1))
              partial = calculatePartial(num, extended)              
            }
            x2 = partial._1 * partial._1
            y2 = partial._2 * partial._2
            //println(num + ": x = " + partial._1 + " y = " + partial._2)
            xValues(num) = partial._1
          }
        }
        val maxX = xValues.max
        val solution = xValues.maxBy(_._2)

        println("Problem 66: " + solution._1)
      }
    }
  }
}