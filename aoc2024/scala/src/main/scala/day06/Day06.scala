package day06

import day06.Direction.{EAST, NORTH, SOUTH, WEST}
import util.{Map2D, Pos, parseMap}

import scala.annotation.tailrec
import scala.io.Source

enum Direction:
  case NORTH, EAST, SOUTH, WEST

  def turnRight: Direction = this match
    case NORTH => EAST
    case EAST => SOUTH
    case SOUTH => WEST
    case WEST => NORTH

extension (pos: Pos)
  def walkStep(direction: Direction): Pos = direction match
    case NORTH => Pos(pos.x, pos.y - 1)
    case EAST => Pos(pos.x + 1, pos.y)
    case SOUTH => Pos(pos.x, pos.y + 1)
    case WEST => Pos(pos.x - 1, pos.y)

def pt1(map: Map2D[Char]): Int =
  val guard = findGuard(map)
  val visited = visit(map, guard, Set(guard))
  visited.size

@tailrec
def visit(map: Map2D[Char], position: Pos, visited: Set[Pos] = Set(), direction: Direction = NORTH): Set[Pos] =
  val newPos = position.walkStep(direction)
  if map.isOutOfBounds(newPos) then visited
  else if map(newPos) == '#' then visit(map, position, visited, direction.turnRight)
  else visit(map, newPos, visited + newPos, direction)

def findGuard(map: Map2D[Char]): Pos =
  map.map.find(_._2 == '^').get._1

def pt2(map: Map2D[Char]): Int =
  val guard = findGuard(map)
  val visited = visit(map, guard, Set(guard))
  visited.count { pos =>
    if map(pos) == '#' then false
    else if pos == guard then false
    else detectLoop(map.update(pos, '#'), guard, Set(guard), Set((guard, NORTH)))
  }

@tailrec
def detectLoop(map: Map2D[Char], position: Pos, visited: Set[Pos], loopDetection: Set[(Pos, Direction)], direction: Direction = NORTH): Boolean =
  val newPos = position.walkStep(direction)
  if loopDetection.contains((newPos, direction)) then true
  else if map.isOutOfBounds(newPos) then false
  else if map(newPos) == '#' then detectLoop(map, position, visited, loopDetection + ((newPos, direction)), direction.turnRight)
  else detectLoop(map, newPos, visited + newPos, loopDetection + ((newPos, direction)), direction)


@main
def main(): Unit =
  val input = Source.fromResource("day06.txt").getLines().map(_.trim).toList
  val map = parseMap(input, identity, '.')
  println(s"pt1: ${pt1(map)}")
  println(s"pt2: ${pt2(map)}")
