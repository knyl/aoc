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
  val (map, width, height) = parseMap(lines)
  val start = map.find(_._2 == 'S').map(_._1).get
  val toVisit = List(start -> 0)
  bfs(toVisit, Set.empty, map, 0)

@tailrec
def bfs(toVisit: List[(Pos, Int)], visited: Set[Pos], map: Map2D, steps: Int): Int = toVisit.headOption match
  case None => steps
  case Some((_, x)) if x > 64 => steps
  case Some((pos, _)) if visited.contains(pos) => bfs(toVisit.tail, visited, map, steps)
  case Some((pos, x)) =>
    //println(s"Visiting $pos at $x")
    val newToVisit = neighbours(pos).diff(visited).filter(map.contains).filter(map(_) == '.').map(_ -> (x + 1))
    if x % 2 == 0 then
      bfs(toVisit.tail ++ newToVisit, visited + pos, map, steps + 1)
    else
      bfs(toVisit.tail ++ newToVisit, visited + pos, map, steps)

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


val example =
  """
    |...........
    |.....###.#.
    |.###.##..#.
    |..#.#...#..
    |....#.#....
    |.##..S####.
    |.##..#...#.
    |.......##..
    |.##.#.####.
    |.##..##.##.
    |...........
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList
