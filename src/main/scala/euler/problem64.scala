package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

//All square roots are periodic when written as continued fractions and can be written in the form:
//
//√N = a0 + 1/(a1 + 1/(a2 + 1/(1 + a3 + ...
//For example, let us consider √23:
//
//√23 = 4 + √23 — 4 = 4 + 1/(1 + (√23 – 3)/7)
//If we continue we would get the following expansion:
//
//√23 = 4 + 1/(1 + 1/(3 + 1/(1 + 1/(8 + 1/...))))
//
//The process can be summarised as follows:
//
//a0 = 4,   
//1
//√23—4
// =  
//√23+4
//7
// = 1 +  
//√23—3
//7
//a1 = 1,   
//7
//√23—3
// =  
//7(√23+3)
//14
// = 3 +  
//√23—3
//2
//a2 = 3,   
//2
//√23—3
// =  
//2(√23+3)
//14
// = 1 +  
//√23—4
//7
//a3 = 1,   
//7
//√23—4
// =  
//7(√23+4)
//7
// = 8 +  √23—4
//a4 = 8,   
//1
//√23—4
// =  
//√23+4
//7
// = 1 +  
//√23—3
//7
//a5 = 1,   
//7
//√23—3
// =  
//7(√23+3)
//14
// = 3 +  
//√23—3
//2
//a6 = 3,   
//2
//√23—3
// =  
//2(√23+3)
//14
// = 1 +  
//√23—4
//7
//a7 = 1,   
//7
//√23—4
// =  
//7(√23+4)
//7
// = 8 +  √23—4
//It can be seen that the sequence is repeating. For conciseness, we use the notation √23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.
//
//The first ten continued fraction representations of (irrational) square roots are:
//
//√2=[1;(2)], period=1
//√3=[1;(1,2)], period=2
//√5=[2;(4)], period=1
//√6=[2;(2,4)], period=2
//√7=[2;(1,1,1,4)], period=4
//√8=[2;(1,4)], period=2
//√10=[3;(6)], period=1
//√11=[3;(3,6)], period=2
//√12= [3;(2,6)], period=2
//√13=[3;(1,1,1,1,6)], period=5
//
//Exactly four continued fractions, for N ≤ 13, have an odd period.
//
//How many continued fractions for N ≤ 10000 have an odd period?

//
// m_0 = 0
// d_0 = 1
// a_0 = floor(sqrt(S))
// m_{n+1} = d_n * a_n - m_n
// d_{n+1} = (S - m_{n+1}^2) / d_n
// a_{n+1} = floor(((sqrt(S) + m_{n+1}))) / d_{n+1}) = floor((a_0 + m_{n+1}) / d_{n+1})
// Notice that mn, dn, and an are always integers.
// The algorithm terminates when this triplet is the same as one encountered before.
// The algorithm can also terminate on ai when ai = 2 a0,[11] which is easier to implement.


object Problem64 {

  def findCycle(s: Int) = {
    var m_n = 0
    var d_n = 1
    val a_0 = sqrt(s).toInt
    var a_n = a_0
    var done = false
    val as = new ArrayBuffer[Int]()
    as += a_0
    
    while (!done) {
      //println(m_n, d_n, a_n)
      val m_next = (d_n * a_n - m_n).toInt
      val d_next = ((s - m_next * m_next) / d_n).toInt
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
    as
  }
  
  def main(args: Array[String]) = {
    time {
      breakable {
        val matches = (2 to 10000) map {findCycle(_).size - 1} filter {_ % 2 == 1}//        val partials = findCycle(13)
        println("Problem 64: " + matches.size)
      }
    }
  }
}