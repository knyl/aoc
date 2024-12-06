package day04

import util.{Map2D, Pos, parseMap}

import scala.io.Source

def pattern1(x: Int, y: Int) = List(
  List((x + 1, y), (x + 2, y), (x + 3, y)),
  List((x + 1, y - 1), (x + 2, y - 2), (x + 3, y - 3)),
  List((x, y - 1), (x, y - 2), (x, y - 3)),
  List((x - 1, y - 1), (x - 2, y - 2), (x - 3, y - 3)),
  List((x - 1, y), (x - 2, y), (x - 3, y)),
  List((x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3)),
  List((x, y + 1), (x, y + 2), (x, y + 3)),
  List((x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3))
)

def matchPt1(pattern: String): Boolean = pattern match
  case "MAS" => true
  case _ => false

def pattern2(x: Int, y: Int) = List(
  List((x + 1, y - 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y + 1))
)

def matchPt2(pattern: String): Boolean = pattern match
  case "MMSS" => true
  case "SMMS" => true
  case "SSMM" => true
  case "MSSM" => true
  case _ => false

def solve(data: Map2D[Char], mainChar: Char, pattern: (Int, Int) => List[List[(Int, Int)]], stringMatch: String => Boolean): Int =
  data.map.map {
    case (Pos(x, y), c) if c == mainChar =>
      pattern(x, y)
        .map(p => p.map(f => data(Pos(f._1, f._2))).mkString)
        .count(stringMatch)
    case _ => 0
  }.sum

def parseInput(line: List[String]): Map2D[Char] =
  parseMap(line, identity, '.')

@main
def main(): Unit =
  val input = Source.fromResource("day04.txt").getLines().toList
  val data = parseInput(input)
  println(s"pt1: ${solve(data, 'X', pattern1, matchPt1)}")
  println(s"pt2: ${solve(data, 'A', pattern2, matchPt2)}")
