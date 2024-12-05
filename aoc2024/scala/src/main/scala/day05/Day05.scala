package day05

import scala.io.Source

type Rules = Map[String, List[String]]

def makePairs(nums: List[String]) =
  for {
    (x, idxX) <- nums.zipWithIndex
    (y, idxY) <- nums.zipWithIndex
    if idxX < idxY
  } yield (x, y)

def getMiddleElement(nums: List[String]) =
  nums(nums.length / 2).toInt

def pt1(rules: Rules, ordering: List[List[String]]): Int =
  ordering
    .filter(isValidOrdering(rules, _))
    .map(getMiddleElement)
    .sum

def isValidOrdering(rules: Rules, ordering: List[String]): Boolean =
  makePairs(ordering)
    .forall { case (x, y) => !rules.getOrElse(y, List()).contains(x) }

def pt2(rules: Rules, ordering: List[List[String]]): Int =
  ordering
    .filterNot(isValidOrdering(rules, _))
    .map { l => l.sortWith((a, b) => rules.getOrElse(a, List()).contains(b)) }
    .map(getMiddleElement)
    .sum

def parseInput(str: String): (Rules, List[List[String]]) =
  val lines = str.split("\n\n").map(_.trim).filterNot(_.isBlank).toList
  val tuples = lines.head
    .split("\n")
    .map { case s"${before}|${after}" => (before, after) }
  val rules = tuples.groupBy(_._1).view.mapValues(_.map(_._2).toList).toMap
  val list = lines.tail.head
    .split("\n")
    .map(_.split(",").map(_.trim).toList)
    .toList
  (rules, list)

@main
def main(): Unit =
  val input = Source.fromResource("day05.txt").mkString
  val (beforeRules, orderings) = parseInput(input)
  println(s"pt1: ${pt1(beforeRules, orderings)}")
  println(s"pt2: ${pt2(beforeRules, orderings)}")
