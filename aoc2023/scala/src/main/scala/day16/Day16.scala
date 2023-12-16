package day16

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

type Map2D = Map[Pos, Char]

case class Pos(x: Int, y: Int)

enum Direction:
  case Up, Down, Left, Right

def pt1(lines: List[String]): Int =
  val (map, width, height) = parseMap(lines)
  val outsideFun = isOutsideFun(width, height)
  val visited = beam(List((Pos(0, 0), Direction.Right)), Set.empty, map, outsideFun)
  calculateEnergy(visited, outsideFun)

def pt2(lines: List[String]): Int =
  val (map, width, height) = parseMap(lines)
  val outsideFun = isOutsideFun(width, height)

  val startTop = (0 until width).map(x => Pos(x, 0)).flatMap(mapAllDirections).toList
  val startBottom = (0 until width).map(x => Pos(x, height - 1)).flatMap(mapAllDirections).toList
  val startLeft = (1 until height - 1).map(y => Pos(0, y)).flatMap(mapAllDirections).toList
  val startRight = (1 until height - 1).map(y => Pos(width - 1, y)).flatMap(mapAllDirections).toList
  val allStartingPositions = startTop ::: startBottom ::: startLeft ::: startRight

  allStartingPositions.map(p => beam(List(p), Set.empty, map, outsideFun))
    .map(calculateEnergy(_, outsideFun))
    .max

def calculateEnergy(tiles: Set[(Pos, Direction)], outsideFun: Pos => Boolean): Int =
  tiles
    .filter { case (pos, _) => !outsideFun(pos) }
    .map { case (pos, _) => pos }
    .toList
    .distinct.length

def mapAllDirections(p: Pos) = Direction.values.map((p, _))

@tailrec
def beam(toVisit: List[(Pos, Direction)], visited: Set[(Pos, Direction)], map: Map2D, outsideFun: Pos => Boolean): Set[(Pos, Direction)] =
  toVisit.headOption match
    case None => visited
    case Some(nextToVisit) if visited.contains(nextToVisit) => beam(toVisit.tail, visited, map, outsideFun)
    case Some((pos, _)) if outsideFun(pos) => beam(toVisit.tail, visited, map, outsideFun)
    case Some((pos, dir)) =>
      val nextDirs = nextDirections(dir, map.getOrElse(pos, '.'))
      val nextToVisit = nextDirs.map(d => (nextPosition(pos, d), d))
      val updatedVisited = visited + ((pos, dir))
      beam(toVisit.tail ::: nextToVisit, updatedVisited, map, outsideFun)

def nextDirections(dir: Direction, c: Char): List[Direction] = c match
  case '/' => dir match
    case Direction.Up => List(Direction.Right)
    case Direction.Down => List(Direction.Left)
    case Direction.Left => List(Direction.Down)
    case Direction.Right => List(Direction.Up)
  case '\\' => dir match
    case Direction.Up => List(Direction.Left)
    case Direction.Down => List(Direction.Right)
    case Direction.Left => List(Direction.Up)
    case Direction.Right => List(Direction.Down)
  case '-' => List(Direction.Right, Direction.Left)
  case '|' => List(Direction.Up, Direction.Down)
  case '.' => List(dir)
  case _ => throw new Exception("Unknown char: " + c)

def nextPosition(pos: Pos, dir: Direction): Pos = dir match
  case Direction.Up => pos.copy(y = pos.y - 1)
  case Direction.Down => pos.copy(y = pos.y + 1)
  case Direction.Left => pos.copy(x = pos.x - 1)
  case Direction.Right => pos.copy(x = pos.x + 1)

def isOutsideFun(width: Int, height: Int)(pos: Pos): Boolean =
  pos.x < 0 || pos.y < 0 || pos.x >= width || pos.y >= height

def parseMap(lines: List[String]): (Map2D, Int, Int) =
  val width = lines.headOption.map(_.length).getOrElse(0)
  val height = lines.length
  val map = lines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.map { (c, x) =>
      Pos(x, y) -> c
    }
  }.toMap
  (map, width, height)

@main
def main(): Unit =
  val lines = Source.fromResource("day16.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
