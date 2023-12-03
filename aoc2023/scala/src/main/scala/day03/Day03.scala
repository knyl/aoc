package day03

import scala.annotation.tailrec
import scala.io.Source

type EngineSchematic = Map[(Int, Int), Char]

type SymbolMatchingFunction = ((Int, Int), EngineSchematic) => Option[(Int, Int)]

def pt1(input: List[String]): Long =
  val engineParts = findEngineParts(getSymbolPosition, input)
  engineParts.map(_._1).sum

def pt2(input: List[String]): Long =
  val engineParts = findEngineParts(getGearPosition, input)
  engineParts.groupBy(_._2).filter(_._2.length > 1)
    .map(_._2.map(_._1).product).sum

def findEngineParts(symbolMatcher: SymbolMatchingFunction, input: List[String]): List[(Long, (Int, Int))] =
  val schematic = parseToMap(input)
  val maxX = input.head.length
  val maxY = input.length
  val indices = (0 to maxY).map(y => (0 to maxX).map((_, y)))
  indices.flatMap(findEngineParts(symbolMatcher, "", None, List(), _, schematic)).toList

@tailrec
def findEngineParts(symbolMatcher: SymbolMatchingFunction, number: String, gearPosition: Option[(Int, Int)], acc: List[(Long, (Int, Int))], positions: Seq[(Int, Int)], schematic: EngineSchematic): List[(Long, (Int, Int))] =
  getPartOrSymbol(positions, schematic) match
    case Some(digit) if digit.isDigit =>
      val newNumber = number + digit
      val updatedGearPosition = gearPosition.orElse(symbolMatcher(positions.head, schematic))
      findEngineParts(symbolMatcher, newNumber, updatedGearPosition, acc, positions.tail, schematic)
    case Some(_) if gearPosition.isDefined => findEngineParts(symbolMatcher, "", None, (number.toLong, gearPosition.get) :: acc, positions.tail, schematic)
    case Some(_) => findEngineParts(symbolMatcher, "", None, acc, positions.tail, schematic)
    case None if gearPosition.isDefined => (number.toLong, gearPosition.get) :: acc
    case None => acc

def getPartOrSymbol(positions: Seq[(Int, Int)], schematic: EngineSchematic): Option[Char] =
  positions.headOption.flatMap(schematic.get)

def getGearPosition(tuple: (Int, Int), schematic: EngineSchematic): Option[(Int, Int)] =
  val adjacentKeys = getAdjacentKeys(tuple)
  adjacentKeys.find(schematic.getOrElse(_, '.') == '*')

def getSymbolPosition(tuple: (Int, Int), schematic: EngineSchematic): Option[(Int, Int)] =
  val adjacentKeys = getAdjacentKeys(tuple)
  adjacentKeys.find(isSymbol(_, schematic))

def isSymbol(key: (Int, Int), schematic: EngineSchematic): Boolean =
  val part = schematic.getOrElse(key, '.')
  !part.isDigit && part != '.'

def getAdjacentKeys(tuple: (Int, Int)): List[(Int, Int)] =
  val (x, y) = tuple
  List(
    (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
    (x - 1, y), (x + 1, y),
    (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
  )

def parseToMap(lines: List[String]): EngineSchematic =
  lines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.map { (char, x) =>
      (x, y) -> char
    }
  }.toMap

@main
def main(): Unit =
  val lines = Source.fromResource("day03.txt").getLines().toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))

val example =
  """
    |467..114..
    |...*......
    |..35..633.
    |......#..+
    |617*.....1
    |.....+.58.
    |..592.....
    |......755.
    |...$.*....
    |.664.598..
    |""".stripMargin.split("\n").toList.filter(s => !s.isBlank)