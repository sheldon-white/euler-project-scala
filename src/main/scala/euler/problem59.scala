package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// Each character on a computer is assigned a unique code and the preferred standard is ASCII
// (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42,
// and lowercase k = 107.
// A modern encryption method is to take a text file, convert the bytes to ASCII,
// then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that
// using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107,
// then 107 XOR 42 = 65.
// For unbreakable encryption, the key is the same length as the plain text message, and the key is made up
// of random bytes. The user would keep the encrypted message and the encryption key in different locations,
// and without both "halves", it is impossible to decrypt the message.
// Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key.
// If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message.
// The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.
// Your task has been made easy, as the encryption key consists of three lower case characters.
// Using cipher.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes,
// and the knowledge that the plain text must contain common English words,
// decrypt the message and find the sum of the ASCII values in the original text.

// var xored = messageChar ^ passwordChar

object Problem59 {

  def readCyphertext = {
    val stream : InputStream = getClass.getResourceAsStream("p059_cipher.txt")
    val bytes = scala.io.Source.fromInputStream(stream).getLines.map(_.split(",")).flatten.map(_.toInt).toVector
    bytes
  }
  
  def xorCypherText(cyphertext: Vector[Int], key: Vector[Char]) = {
     (0 until cyphertext.length) map {i => cyphertext(i) ^ key(i % key.length)}
  }
  
  def testAllPasswords(cyphertext: Vector[Int]): Int = {
    for (c1 <- 'a' to 'z'; c2 <- 'a' to 'z'; c3 <- 'a' to 'z') {
      val key = Vector(c1, c2, c3)
      val xored = xorCypherText(cyphertext, key).map(_.toChar).mkString("")
      if (xored.contains(" the ")) {
        println(key.mkString(""))
        val sum = xored.toList.foldLeft(0)(_ + _)
        return sum
      }
    }
    0
  }
  
  def main(args: Array[String]) = {
    time {
      val t = readCyphertext
      println("Problem 59: " + testAllPasswords(t))
    }
  }
}