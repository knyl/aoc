package day04

import util.{Map2D, parseMap}

import scala.io.Source

type Pos = (Int, Int)

def pattern(x: Int, y: Int) = List(
  List((x + 1, y), (x + 2, y), (x + 3, y)),
  List((x + 1, y - 1), (x + 2, y - 2), (x + 3, y - 3)),
  List((x, y - 1), (x, y - 2), (x, y - 3)),
  List((x - 1, y - 1), (x - 2, y - 2), (x - 3, y - 3)),
  List((x - 1, y), (x - 2, y), (x - 3, y)),
  List((x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3)),
  List((x, y + 1), (x, y + 2), (x, y + 3)),
  List((x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3))
)

def pattern2(x: Int, y: Int) = List(
  List((x + 1, y - 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y + 1))
)


def pt1(data: Map2D[Char]): Int =
  data.map.map((pos, value) => {
    if data(pos) != 'X' then 0
    else
      val (x, y) = pos
      val patterns = pattern(x, y)
      val matches = patterns.map(pattern => pattern.map(data(_)).mkString).count(_ == "MAS")
      matches
  }).sum

def pt2(data: Map2D[Char]): Int =
  data.map.map((pos, value) => {
    if data(pos) != 'A' then 0
    else
      val (x, y) = pos
      val patterns = pattern2(x, y)
      val matches = patterns.map(pattern => pattern.map(data(_)).mkString).count(matchPt2)
      matches
  }).sum


def matchPt2(pattern: String): Boolean = pattern match
  case "MMSS" => true
  case "SMMS" => true
  case "SSMM" => true
  case "MSSM" => true
  case _ => false

def parseInput(line: List[String]): Map2D[Char] =
  parseMap(line, identity, '.')

@main
def main(): Unit =
  val input = Source.fromResource("day04.txt").getLines().toList
  val data = parseInput(input)
  println(s"pt1: ${pt1(data)}")
  println(s"pt2: ${pt2(data)}")