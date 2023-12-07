package day07

import scala.io.Source

type Cards = List[Card]

case class Hand(bid: Long, strength: Strength, cards: Cards) extends Ordered[Hand]:
  override def compare(that: Hand): Int =
    if this.strength == that.strength then
      val firstDiffering = this.cards.zip(that.cards).filter((c1, c2) => c1 != c2).head
      firstDiffering._1.ordinal.compareTo(firstDiffering._2.ordinal)
    else
      this.strength.ordinal.compareTo(that.strength.ordinal)

enum Card:
  case Joker, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace

enum Strength:
  case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

def camelCards(lines: List[String], hasJoker: Boolean): Long =
  val hands = createHands(lines, hasJoker)
  hands.sorted.zipWithIndex.map { (hand, index) =>
    hand.bid * (index + 1)
  }.sum

def createHands(lines: List[String], hasJoker: Boolean = false): List[Hand] =
  lines.map {
    case s"$cardsString $bid" =>
      val cards = cardsString.map(toCard(_, hasJoker)).toList
      Hand(bid.toLong, getStrength(cards), cards)
    case line => throw new Exception(s"Couldn't parse line $line")
  }

def toCard(c: Char, hasJoker: Boolean = false): Card =
  c match
    case '2' => Card.Two
    case '3' => Card.Three
    case '4' => Card.Four
    case '5' => Card.Five
    case '6' => Card.Six
    case '7' => Card.Seven
    case '8' => Card.Eight
    case '9' => Card.Nine
    case 'T' => Card.Ten
    case 'J' if !hasJoker => Card.Jack
    case 'J' if hasJoker => Card.Joker
    case 'Q' => Card.Queen
    case 'K' => Card.King
    case 'A' => Card.Ace
    case c => throw new Exception(s"Couldn't parse card $c")


def getStrength(cards: Cards): Strength =
  val cardsGrouped = cards.filter(_ != Card.Joker).groupBy(identity).values.toList
  val jokers = cards.count(_ == Card.Joker)

  if isFiveOfAKind(cardsGrouped, jokers) then Strength.FiveOfAKind
  else if isFourOfAKind(cardsGrouped, jokers) then Strength.FourOfAKind
  else if isFullHouse(cardsGrouped, jokers) then Strength.FullHouse
  else if isThreeOfAKind(cardsGrouped, jokers) then Strength.ThreeOfAKind
  else if isTwoPair(cardsGrouped) then Strength.TwoPair
  else if isPair(cardsGrouped, jokers) then Strength.OnePair
  else Strength.HighCard

def isFiveOfAKind(cards: List[Cards], jokers: Int = 0): Boolean =
  cards.exists(_.length == 5) || (jokers == 1 && isFourOfAKind(cards))
    || (jokers == 2 && isThreeOfAKind(cards)) || (jokers == 3 && isPair(cards))
    || jokers >= 4

def isFourOfAKind(cards: List[Cards], jokers: Int = 0): Boolean =
  cards.exists(_.length == 4) || (jokers == 1 && isThreeOfAKind(cards))
    || (jokers == 2 && isPair(cards)) || jokers == 3

def isFullHouse(cards: List[Cards], jokers: Int = 0): Boolean =
  (cards.exists(_.length == 3) && cards.exists(_.length == 2)) ||
    (jokers == 1 && isTwoPair(cards)) || (jokers == 2 && isPair(cards)) || jokers == 3

def isThreeOfAKind(cards: List[Cards], jokers: Int = 0): Boolean =
  cards.exists(_.length == 3) || (jokers == 1 && isPair(cards)) || jokers == 2

def isTwoPair(cards: List[Cards]): Boolean =
  cards.count(_.length == 2) == 2

def isPair(cards: List[Cards], jokers: Int = 0): Boolean =
  cards.exists(_.length == 2) || jokers == 1

@main
def main(): Unit =
  val lines = Source.fromResource("day07.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + camelCards(lines, false))
  println("Pt1: " + camelCards(lines, true))