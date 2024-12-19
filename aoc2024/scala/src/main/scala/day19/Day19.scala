package day19

import scala.io.Source

def pt1(patterns: List[String], designs: List[String]): Int =
  val sortedPatterns = patterns.sortBy(_.length).reverse
  val filteredPatterns = filterPatterns(sortedPatterns)
  designs.count(d => isPossible(filteredPatterns)(d))

def filterPatterns(patterns: List[String]): List[String] =
  val singleDigitPatterns = patterns.filter(_.length == 1).map(_.head).toSet
  patterns.filterNot(p => p.length > 1 && p.forall(singleDigitPatterns.contains))

def isPossible(patterns: List[String])(design: String): Boolean =
  if design.isEmpty then true
  else
    patterns
      .filter(design.startsWith)
      .exists(m => isPossible(patterns)(design.drop(m.length)))

def pt2(patterns: List[String], designs: List[String]): BigInt =
  designs.map(countDesign(patterns, _)).map(_._2).sum

def countDesign(patterns: List[String], design: String, cache: Map[String, BigInt] = Map()): (Map[String, BigInt], BigInt) =
  if design.isEmpty then (cache, BigInt(1))
  else if cache.contains(design) then (cache, cache(design))
  else
    val (newCache, newCount) = patterns
      .filter(design.startsWith)
      .foldLeft((cache, BigInt(0)))((acc, p) =>
        val res = countDesign(patterns, design.drop(p.length), acc._1)
        (res._1, res._2 + acc._2)
      )
    (newCache.updated(design, newCount), newCount)


def parseInput(strings: List[String]): (List[String], List[String]) =
  (strings.head.split(",").map(_.trim).toList, strings.last.split("\n").toList)

@main
def main(): Unit =
  val input = Source.fromResource("day19.txt").mkString.split("\n\n").map(_.trim).filterNot(_.isBlank).toList
  val (patterns, designs) = parseInput(input)
  println(s"pt1: ${pt1(patterns, designs)}")
  println(s"pt2: ${pt2(patterns, designs)}")
