package day17

import day17.Direction.{Down, Up, Left, Right}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.language.postfixOps

type Map2D = Map[Pos, Int]

case class Pos(x: Int, y: Int)

enum Direction:
  case Up, Down, Left, Right

case class State(pos: Pos, steps: Int, heatLoss: Int, direction: Direction) extends Ordered[State]:
  def compare(that: State): Int = -heatLoss.compare(that.heatLoss)

def pt1(lines: List[String]): Int =
  val (map, width, height) = parseMap(lines)
  val toVisit = mutable.PriorityQueue[State](State(Pos(0, 1), 1, map(Pos(0, 1)), Down), State(Pos(1, 0), 1, map(Pos(1, 0)), Right))
  dijkstra(endPt1(width, height), neighbours, toVisit, Set.empty, map, width, height)

def pt2(lines: List[String]): Int =
  val (map, width, height) = parseMap(lines)
  val toVisit = mutable.PriorityQueue[State](State(Pos(0, 1), 1, map(Pos(0, 1)), Down), State(Pos(1, 0), 1, map(Pos(1, 0)), Right))
  dijkstra(endPt2(width, height), neighbours2, toVisit, Set.empty, map, width, height)

@tailrec
def dijkstra(endFun: State => Boolean,
             neighbourFun: (Pos, Int, Direction) => List[(Pos, Int, Direction)],
             toVisit: mutable.PriorityQueue[State],
             visited: Set[(Pos, Int, Direction)],
             map: Map2D,
             width: Int, height: Int): Int =
  toVisit.dequeue() match
    case state if endFun(state) => state.heatLoss
    case State(pos, steps, _, direction) if visited.contains((pos, steps, direction)) => dijkstra(endFun, neighbourFun, toVisit, visited, map, width, height)
    case State(pos, consecutiveSteps, heatLoss, direction) =>
      val newSteps = neighbourFun(pos, consecutiveSteps, direction).filterNot(p => isOutside(p._1, width, height)).filterNot(visited.contains)
      val newStates = newSteps.map((p, steps, dir) => State(p, steps, heatLoss + map(p), dir))
      val updatedVisited = visited + ((pos, consecutiveSteps, direction))
      dijkstra(endFun, neighbourFun, toVisit ++ newStates, updatedVisited, map, width, height)

def isOutside(pos: Pos, width: Int, height: Int) =
  pos.x < 0 || pos.y < 0 || pos.x >= width || pos.y >= height

def neighbours(pos: Pos, steps: Int, direction: Direction): List[(Pos, Int, Direction)] =
  if steps == 2 then List(turnLeft(pos, direction), turnRight(pos, direction))
  else List(turnLeft(pos, direction), turnRight(pos, direction), goStraight(pos, steps, direction))

def neighbours2(pos: Pos, steps: Int, direction: Direction): List[(Pos, Int, Direction)] =
  if steps == 9 then List(turnLeft(pos, direction), turnRight(pos, direction))
  else if steps < 3 then List(goStraight(pos, steps, direction))
  else List(turnLeft(pos, direction), turnRight(pos, direction), goStraight(pos, steps, direction))

def endPt1(width: Int, height: Int)(state: State): Boolean = state.pos.x == width - 1 && state.pos.y == height - 1
def endPt2(width: Int, height: Int)(state: State): Boolean = state.pos.x == width - 1 && state.pos.y == height - 1 && state.steps > 2

def turnLeft(pos: Pos, direction: Direction): (Pos, Int, Direction) = direction match
  case Up => (Pos(pos.x - 1, pos.y), 0, Left)
  case Down => (Pos(pos.x + 1, pos.y), 0, Right)
  case Left => (Pos(pos.x, pos.y + 1), 0, Down)
  case Right => (Pos(pos.x, pos.y - 1), 0, Up)

def turnRight(pos: Pos, direction: Direction): (Pos, Int, Direction) = direction match
  case Up => (Pos(pos.x + 1, pos.y), 0, Right)
  case Down => (Pos(pos.x - 1, pos.y), 0, Left)
  case Left => (Pos(pos.x, pos.y - 1), 0, Up)
  case Right => (Pos(pos.x, pos.y + 1), 0, Down)

def goStraight(pos: Pos, steps: Int, direction: Direction): (Pos, Int, Direction) = direction match
  case Up => (Pos(pos.x, pos.y - 1), steps + 1, Up)
  case Down => (Pos(pos.x, pos.y + 1), steps + 1, Down)
  case Left => (Pos(pos.x - 1, pos.y), steps + 1, Left)
  case Right => (Pos(pos.x + 1, pos.y), steps + 1, Right)

def parseMap(lines: List[String]): (Map2D, Int, Int) =
  val width = lines.headOption.map(_.length).getOrElse(0)
  val height = lines.length
  val map = lines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.map { (c, x) =>
      Pos(x, y) -> c.asDigit
    }
  }.toMap
  (map, width, height)

@main
def main(): Unit =
  val lines = Source.fromResource("day17.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))

