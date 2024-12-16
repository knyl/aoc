package day16

import day16.Direction.{EAST, NORTH, SOUTH, WEST}
import util.{Map2D, Pos, parseMap}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

enum Direction:
  case NORTH, EAST, SOUTH, WEST

extension (pos: Pos)
  def forward(direction: Direction): Pos = direction match
    case Direction.NORTH => Pos(pos.x, pos.y - 1)
    case Direction.EAST => Pos(pos.x + 1, pos.y)
    case Direction.SOUTH => Pos(pos.x, pos.y + 1)
    case Direction.WEST => Pos(pos.x - 1, pos.y)

case class State(pos: Pos, cost: Int, direction: Direction) extends Ordered[State]:
  def turns: List[State] = direction match
    case Direction.NORTH =>
      List(State(Pos(pos.x - 1, pos.y), cost + 1001, WEST), State(Pos(pos.x + 1, pos.y), cost + 1001, EAST))
    case Direction.EAST =>
      List(State(Pos(pos.x, pos.y - 1), cost + 1001, NORTH), State(Pos(pos.x, pos.y + 1), cost + 1001, SOUTH))
    case Direction.SOUTH =>
      List(State(Pos(pos.x - 1, pos.y), cost + 1001, WEST), State(Pos(pos.x + 1, pos.y), cost + 1001, EAST))
    case Direction.WEST =>
      List(State(Pos(pos.x, pos.y - 1), cost + 1001, NORTH), State(Pos(pos.x, pos.y + 1), cost + 1001, SOUTH))

  def forward: State = State(pos.forward(direction), cost + 1, direction)

  override def compare(that: State): Int = that.cost.compareTo(cost)

def pt1(map: Map2D[Char]): Int =
  val startNode = map.map.find(_._2 == 'S').get._1
  val queue = mutable.PriorityQueue(State(startNode, 0, Direction.EAST))
  val end = map.map.find(_._2 == 'E').get._1
  findPath(map, queue, end)

@tailrec
def findPath(map: Map2D[Char], queue: mutable.PriorityQueue[State], end: Pos, visited: Set[(Pos, Direction)] = Set()): Int =
  val next = queue.dequeue()
  if next.pos == end then next._2
  else if visited.contains((next.pos, next.direction)) then findPath(map, queue, end, visited)
  else
    val neighbours = (next.forward :: next.turns)
      .filterNot(s => map.isOutOfBounds(s.pos))
      .filterNot(s => map(s.pos) == '#')
    neighbours.foreach(queue.enqueue(_))
    findPath(map, queue, end, visited + ((next.pos, next.direction)))

def pt2(map: Map2D[Char]): Int =
  val startNode = map.map.find(_._2 == 'S').get._1
  val queue = mutable.PriorityQueue(State(startNode, 0, Direction.EAST))
  val end = map.map.find(_._2 == 'E').get._1
  val (endNode, prevMap) = findPath2(map, queue, end)
  val allPositions = getScore(prevMap, endNode)
  allPositions.toSet.size + 1

def getScore(previous: Map[State, List[State]], curr: State): List[Pos] =
  previous(curr)
    .flatMap {
      case state if state.cost == 0 => state.pos :: Nil
      case state => state.pos :: getScore(previous, state)
    }


@tailrec
def findPath2(map: Map2D[Char], queue: mutable.PriorityQueue[State], end: Pos, visited: Set[(Pos, Direction)] = Set(), previous: Map[State, List[State]] = Map()): (State, Map[State, List[State]]) =
  val next = queue.dequeue()
  if next.pos == end then (next, previous)
  else if visited.contains((next.pos, next.direction)) then findPath2(map, queue, end, visited, previous)
  else
    val neighbours = (next.forward :: next.turns)
      .filterNot(s => map.isOutOfBounds(s.pos))
      .filterNot(s => map(s.pos) == '#')

    neighbours.foreach(queue.enqueue(_))
    val updatedPrevious = neighbours.foldLeft(previous)((acc, s) => acc + ((s, next :: acc.getOrElse(s, Nil))))
    findPath2(map, queue, end, visited + ((next.pos, next.direction)), updatedPrevious)

@main
def main(): Unit =
  val input = Source.fromResource("day16.txt").getLines().map(_.trim).toList
  val map = parseMap(input, identity, '-')
  println(s"pt1: ${pt1(map)}")
  println(s"pt2: ${pt2(map)}")