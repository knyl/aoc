package day05

import scala.annotation.tailrec
import scala.io.Source

case class Range(start: Long, end: Long):
  def contains(value: Long): Boolean =
    value >= start && value <= end
  def overlaps(other: Range): Boolean =
    this.start <= other.end && other.start <= this.end
  def numbersInRange: Long =
    end - start + 1

def pt1(fresh: List[Range], available: List[Long]): Long =
  available
    .count(ingredient => fresh.exists(_.contains(ingredient)))

def pt2(fresh: List[Range]): Long =
  val mergedFresh = merge(fresh)
  mergedFresh.map(_.numbersInRange).sum

@tailrec
def merge(ranges: List[Range], parsedRanges: List[Range] = Nil): List[Range] =
  ranges match
    case Nil => parsedRanges
    case a :: tail =>
      val (fullyMergedRange, updatedRanges) = mergeRanges(a, tail)
      merge(updatedRanges, fullyMergedRange :: parsedRanges)

@tailrec
def mergeRanges(a: Range, ranges: List[Range], parsedRanges: List[Range] = Nil): (Range, List[Range]) = ranges match
  case Nil => (a, parsedRanges)
  case b :: tail =>
    if a.overlaps(b) then
      val newRange = Range(Math.min(a.start, b.start), Math.max(a.end, b.end))
      mergeRanges(newRange, tail ++ parsedRanges)
    else mergeRanges(a, tail, b :: parsedRanges)


def parseFresh(input: List[String]): List[Range] =
  input
    .map(_.trim)
    .filterNot(_.isBlank)
    .map( line =>
      val parts = line.split("-")
      Range(parts(0).toLong, parts(1).toLong)
    )

def parseAvailable(input: List[String]): List[Long] =
  input
    .map(_.trim)
    .filterNot(_.isBlank)
    .map(_.toLong)

def parseInput(strings: List[String]): (List[Range], List[Long]) =
  val freshInput = strings.takeWhile(_ != "")
  val availableInput = strings.dropWhile(_ != "").drop(1)
  (parseFresh(freshInput), parseAvailable(availableInput))

@main
def main(): Unit =
  val input = Source.fromResource("day05.txt").getLines().toList
  val (fresh, available) = parseInput(input)

  println("Pt1: " + pt1(fresh, available))
  println("Pt2: " + pt2(fresh))

val example =
  """
    |3-5
    |10-14
    |16-20
    |12-18
    |
    |1
    |5
    |8
    |11
    |17
    |32
    |""".stripMargin.split("\n").map(_.trim).toList