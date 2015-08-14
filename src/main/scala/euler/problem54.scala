package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:
// High Card: Highest value card.
// One Pair: Two cards of the same value.
// Two Pairs: Two different pairs.
// Three of a Kind: Three cards of the same value.
// Straight: All cards are consecutive values.
// Flush: All cards of the same suit.
// Full House: Three of a kind and a pair.
// Four of a Kind: Four cards of the same value.
// Straight Flush: All cards are consecutive values of same suit.
// Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
// The cards are valued in the order: 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
// If two players have the same ranked hands then the rank made up of the highest value wins;
// for example, a pair of eights beats a pair of fives (see example 1 below).
// But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared
// (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.
// The file, poker.txt, contains one-thousand random hands dealt to two players.
// Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards
// and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards),
// each player's hand is in no specific order, and in each hand there is a clear winner.
// How many hands does Player 1 win?

object Problem54 {
  object HandType extends Enumeration {
    type HandType = Value
    val HIGH_CARD, PAIR, TWO_PAIR, THREE_OF_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_KIND, STRAIGHT_FLUSH, ROYAL_FLUSH = Value
    
    val rank = HashMap(
        HIGH_CARD -> 1,
        PAIR -> 2,
        TWO_PAIR -> 3,
        THREE_OF_KIND -> 4,
        STRAIGHT -> 5,
        FLUSH -> 6,
        FULL_HOUSE -> 7,
        FOUR_OF_KIND -> 8,
        STRAIGHT_FLUSH -> 9,
        ROYAL_FLUSH -> 10)
  }
  import HandType._
  
  case class Hand(handType: HandType, highCard: Int) extends Ordered[Hand] {
    def compare(that: Hand) = {
      val rdiff = HandType.rank(this.handType) - HandType.rank(that.handType)
      rdiff match {
        case 0 => this.highCard - that.highCard
        case _ => rdiff 
        }
      }
  }
  
  def evaluateCards(cardStrs: Vector[String]): Hand = {
    val rankMap = HashMap(
        '2' -> 2,
        '3' -> 3,
        '4' -> 4,
        '5' -> 5,
        '6' -> 6,
        '7' -> 7,
        '8' -> 8,
        '9' -> 9,
        'T' -> 10,
        'J' -> 11,
        'Q' -> 12,
        'K' -> 13,
        'A' -> 14)
        
    def isFlush(groupedSuits: Vector[Int]) = {groupedSuits == Vector(5)}

    def isStraight(ranks: Vector[Int]) = {
      ranks.map(_ - ranks.last) == List(-4, -3, -2, -1, 0)
    }
    
    def isFullHouse(groupedRanks: Vector[Int]) = {groupedRanks == Vector(2, 3)}
    def isTwoPair(groupedRanks: Vector[Int]) = {groupedRanks == Vector(1, 2, 2)}
    def isPair(groupedRanks: Vector[Int]) = {groupedRanks == Vector(1, 1, 1, 2)}
    def isThreeOfKind(groupedRanks: Vector[Int]) = {groupedRanks == Vector(1, 1, 3)}
    def isFourOfKind(groupedRanks: Vector[Int]) = {groupedRanks == Vector(1, 4)}
     
    def highCard(ranks: Vector[Int]) = {
      val m = ranks.groupBy(_.toString).values.toVector.sortBy(v => (v.size, v(0))).reverse
      m(0)(0)
    }
    
    val ranks = (for (s <- cardStrs) yield s.toList(0)) map {rankMap(_)} sorted
    val groupedRanks = ranks.groupBy(_.toString).values.map(_.size).toVector.sorted
    val suits = for (s <- cardStrs) yield s.toList(1)
    val groupedSuits = suits.groupBy(_.toString).values.map(_.size).toVector.sorted
    
    (ranks, suits) match {
      case r if isStraight(ranks) && isFlush(groupedSuits) && highCard(ranks) == 14 => {Hand(ROYAL_FLUSH, highCard(ranks))}
      case r if isStraight(ranks) && isFlush(groupedSuits) => {Hand(STRAIGHT_FLUSH, highCard(ranks))}
      case r if isFourOfKind(groupedRanks) => {Hand(FOUR_OF_KIND, highCard(ranks))}
      case r if isFullHouse(groupedRanks) => {Hand(FULL_HOUSE, highCard(ranks))}
      case r if isFlush(groupedSuits) => {Hand(FLUSH, highCard(ranks))}
      case r if isStraight(ranks) => {Hand(STRAIGHT, highCard(ranks))}
      case r if isThreeOfKind(groupedRanks) => {Hand(THREE_OF_KIND, highCard(ranks))}
      case r if isTwoPair(groupedRanks) => {Hand(TWO_PAIR, highCard(ranks))}
      case r if isPair(groupedRanks) => {Hand(PAIR, highCard(ranks))}
      case _ => {Hand(HIGH_CARD, highCard(ranks))}
    }
  }
  
  def readAllHands() = {
    val stream : InputStream = getClass.getResourceAsStream("p054_poker.txt")
    val lines = scala.io.Source.fromInputStream(stream).getLines
    val allHands = for (line <- lines) yield buildHands(line)
    allHands
  }
  
  def buildHands(line: String) = {
    val cardStrs = line.split(" ").toVector
    val h1 = evaluateCards(cardStrs.slice(0, 5).toVector)
    val h2 = evaluateCards(cardStrs.slice(5, 10).toVector)
    (h1, h2)
  }

  def main(args: Array[String]) = {
    time {
      val count = readAllHands.filter(p => p._1 > p._2).size
      println("Problem 54: " + count)
    }
  }
}
