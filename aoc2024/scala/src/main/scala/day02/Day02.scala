package day02

import scala.io.Source

def pt1(line: List[Int]): Boolean =
  val resultInc = line.sliding(2).forall(l => l.last > l.head && l.last < (l.head + 4))
  val resultDec = line.sliding(2).forall(l => l.head > l.last && l.head < (l.last + 4))
  resultInc || resultDec

def pt2(line: List[Int]): Boolean =
  if pt1(line) then
    true
  else
    tryDampener(line)

def tryDampener(list: List[Int]): Boolean =
  list.indices.map(i => list.patch(i, Nil, 1)).exists(pt1)

def parseInput(line: List[String]): List[List[Int]] =
  line.map(_.split("\\s+").map(_.toInt).toList)

@main
def main(): Unit =
  val input = Source.fromResource("day02.txt").getLines().map(_.trim).filterNot(_.isBlank).toList
  val lines = parseInput(input)
  println("Pt1: " + lines.map(pt1).count(_ == true))
  println("Pt2: " + lines.map(pt2).count(_ == true))
