package day04


import scala.annotation.tailrec
import scala.io.Source

type Card = (List[Int], List[Int])

def gamePt1(line: String): Int =
  val (winningNumbers, scratchedNumbers) = parseGame(line)
  val sum = scratchedNumbers.count(winningNumbers.contains)
  Math.pow(2, sum - 1).toInt

def gamePt2(lines: List[String]): Int =
  val cards = lines.map(parseGame)
  val cardAmounts = cards.zipWithIndex.map(_._2).map((_, 1)).toMap
  processCards(cards.zipWithIndex, cardAmounts)

@tailrec
def processCards(cards: List[(Card, Int)], cardAmounts: Map[Int, Int]): Int = cards.headOption match
  case Some(((winningNumbers, scratchedNumbers), cardNumber)) =>
    val cardCount = cardAmounts.getOrElse(cardNumber, 1)
    val winningNumberCount = scratchedNumbers.count(winningNumbers.contains)
    val newCards = (cardNumber + 1) to (cardNumber + winningNumberCount)
    val updatedCardAmounts = newCards.map(n => (n, cardAmounts.getOrElse(n, 1) + cardCount)).toList
    processCards(cards.tail, cardAmounts ++ updatedCardAmounts)
  case None => cardAmounts.values.sum

def parseGame(line: String): Card =
  line match
    case s"Card $x: $winningNumberString | $scratchedNumberString" =>
      val winningNumbers = winningNumberString.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toList
      val scratchedNumbers = scratchedNumberString.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toList
      (winningNumbers, scratchedNumbers)
    case _ => throw new Exception(s"Invalid line: $line")
@main
def main(): Unit =
  val lines = Source.fromResource("day04.txt").getLines().toList

  println("Pt1: " + lines.map(gamePt1).sum)
  println("Pt2: " + gamePt2(lines))

val example =
  """
    |Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    |""".stripMargin.split("\n").toList.filter(s => !s.isBlank)
