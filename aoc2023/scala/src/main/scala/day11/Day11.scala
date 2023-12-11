package day11

import util.manhattan_distance

import scala.annotation.tailrec
import scala.io.Source

type Galaxies = List[(Long, Long)]

def galaxyDistances(lines: List[String], empty: Int): BigInt =
  val emptyColumns = lines.transpose.zipWithIndex.filter(_._1.forall(_ == '.')).map(_._2)
  val galaxies = parseLines(lines, emptyColumns, empty).map(p => (BigInt(p._1), BigInt(p._2)))
  val galaxyPairs = galaxies.combinations(2).toList
  galaxyPairs.map(l => (l.head, l.last)).map(manhattan_distance).sum

@tailrec
def parseLines(lines: List[String], emptyColumns: List[Int], emptyCount: Int, row: Int = 0, galaxies: Galaxies = List()): Galaxies = lines.headOption match
  case None => galaxies
  case Some(line) if line.exists(_ == '#') =>
    val newGalaxies = parseRow(line.zipWithIndex, emptyColumns, emptyCount, row)
    parseLines(lines.tail, emptyColumns, emptyCount, row + 1, galaxies ++ newGalaxies)
  case Some(_) => parseLines(lines.tail, emptyColumns, emptyCount, row + emptyCount, galaxies)

@tailrec
def parseRow(line: Seq[(Char, Int)], emptyColumns: List[Int], emptyCount: Int, row: Int, column: Int = 0, galaxies: Galaxies = List()): Galaxies = line.headOption match
  case None => galaxies
  case Some(('#', _)) => parseRow(line.tail, emptyColumns, emptyCount, row, column + 1, (column, row) :: galaxies)
  case Some((_, i)) if emptyColumns.contains(i) => parseRow(line.tail, emptyColumns, emptyCount, row, column + emptyCount, galaxies)
  case _ => parseRow(line.tail, emptyColumns, emptyCount, row, column + 1, galaxies)

@main
def main(): Unit =
  val lines = Source.fromResource("day11.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + galaxyDistances(lines, 2))
  println("Pt2: " + galaxyDistances(lines, 1000000))
