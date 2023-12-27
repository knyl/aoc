package day21

import day21.Direction.{Down, Left, Right, Up}

import scala.annotation.{tailrec, targetName}
import scala.io.Source
import scala.language.postfixOps

type Map2D = Map[Pos, Char]

case class Pos(x: Int, y: Int):
  @targetName("addPos")
  def +(other: Pos): Pos = Pos(x + other.x, y + other.y)

  @targetName("addDirection")
  def +(d: Direction): Pos = d match
    case Direction.Up => Pos(x, y - 1)
    case Direction.Down => Pos(x, y + 1)
    case Direction.Left => Pos(x - 1, y)
    case Direction.Right => Pos(x + 1, y)

case class State(pos: Pos, steps: Int) extends Ordered[State]:
  def compare(that: State): Int = -steps.compare(that.steps)

enum Direction:
  case Up, Down, Left, Right

def pt1(lines: List[String]): Int =
  val (map, width, _) = parseMap(lines)
  val start = map.find(_._2 == 'S').map(_._1).get
  val toVisit = List(start -> 0)
  bfs(toVisit, Set.empty, map, 0, 64, width, 0)

def pt2(lines: List[String]) =
  val (map, width, height) = parseMap(lines)
  val start = map.find(_._2 == 'S').map(_._1).get
  val toVisit = List(start -> 0)
  val steps = 26501365
  val n = steps / height

  val n0 = bfs(toVisit, Set.empty, map, 0, 65, width, 1)
  val n1 = bfs(toVisit, Set.empty, map, 0, 65 + 131, width, 0)
  val n2 = bfs(toVisit, Set.empty, map, 0, 65 + 131 * 2, width, 1)
  // y = ax^2 + bx + c
  // n0 = a * 0^2 + b * 0 + c
  // => n0 = c
  // n1 = a * 1^2 + b * 1 + c
  // => n1 = a + b + n0
  // => a = n1 - n0 - b
  // n2 = a * 2^2 + b * 2 + c
  // => n2 = 4a + 2b + n0
  // b = (n2 - n0 - 4a) / 2
  // => b = (n2 - n0 - 4(n1 - n0 - b)) / 2
  // => b = (n2 - n0 - 4n1 + 4n0 + 4b) / 2
  // => b = n2/2 - n0/2 - 2n1 + 2n0 + 2b
  // => -b = n2/2 - n0/2 - 2n1 + 2n0
  // => b = n0/2 - n2/2 + 2n1 - 2n0
  val b = n0 / 2 - n2 / 2 + 2 * n1 - 2 * n0
  val a = n1 - n0 - b

  val polynomial = (n: BigDecimal) => a * n * n + b * n + n0
   polynomial(BigDecimal(n))

def isPlot(pos: Pos, map: Map2D, width: Int): Boolean =
  val x = if pos.x % width < 0 then pos.x % width + width else pos.x % width
  val y = if pos.y % width < 0 then pos.y % width + width else pos.y % width
  map(Pos(x, y)) == '.' || map(Pos(x, y)) == 'S'

@tailrec
def bfs(toVisit: List[(Pos, Int)], visited: Set[Pos], map: Map2D, steps: Int, maxSteps: Int, width: Int, evenOdd: Int): Int = toVisit.headOption match
  case None => steps
  case Some((_, x)) if x > maxSteps => steps
  case Some((pos, _)) if visited.contains(pos) => bfs(toVisit.tail, visited, map, steps, maxSteps, width, evenOdd)
  case Some((pos, x)) =>
    val newToVisit = neighbours(pos).diff(visited).filter(p => isPlot(p, map, width)).map(_ -> (x + 1))
    if x % 2 == evenOdd then bfs(toVisit.tail ++ newToVisit, visited + pos, map, steps + 1, maxSteps, width, evenOdd)
    else bfs(toVisit.tail ++ newToVisit, visited + pos, map, steps, maxSteps, width, evenOdd)

def neighbours(p: Pos): Set[Pos] =
  Set(p + Up, p + Down, p + Left, p + Right)

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
  val lines = Source.fromResource("day21.txt").getLines().map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
