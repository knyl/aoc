package day11

import scala.io.Source

def galaxyDistances(lines: List[String], empty: Int): BigInt =
  val galaxies = parseInput(lines, empty)
  val galaxyPairs = galaxies.combinations(2).toList
  galaxyPairs.map(l => (l.head, l.last)).map(distance).sum

def distance(g1: (BigInt, BigInt), g2: (BigInt, BigInt)): BigInt =
  (g1._1 - g2._1).abs + (g1._2 - g2._2).abs

def parseInput(lines: List[String], empty: Int): List[(BigInt, BigInt)] =
  val emptyColumns = lines.transpose.zipWithIndex.filter(_._1.forall(_ == '.')).map(_._2)
  var y = 0
  var galaxies: List[(Int, Int)] = List()
  for line <- lines yield
    if line.exists(_ != '.') then
      var x = 0
      for (c, i) <- line.toCharArray.zipWithIndex yield
        if c == '#' then galaxies = (x, y) :: galaxies
        if emptyColumns.contains(i) then x += empty else x += 1
      y += 1
    else y += empty
  galaxies.map(p => (BigInt(p._1), BigInt(p._2)))

@main
def main(): Unit =
  val lines = Source.fromResource("day11.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + galaxyDistances(lines, 2))
  println("Pt2: " + galaxyDistances(lines, 1000000))
