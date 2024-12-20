package day20


import util.{Map2D, Pos, parseMap}

import scala.annotation.tailrec
import scala.io.Source

extension (map: Map2D[Char])
  def neighbours(pos: Pos): List[Pos] =
    List(
      Pos(pos.x, pos.y - 1),
      Pos(pos.x + 1, pos.y),
      Pos(pos.x, pos.y + 1),
      Pos(pos.x - 1, pos.y)
    ).filterNot(map.isOutOfBounds)

case class State(pos: Pos, currentPath: List[Pos] = List()):
  def step(newPos: Pos): State = State(newPos, pos :: currentPath)

@tailrec
def findPathNoCheat(map: Map2D[Char], toVisit: Set[State]): List[Pos] =
  if toVisit.isEmpty then Nil
  else if map(toVisit.head.pos) == 'E' then toVisit.head.currentPath
  else
    val next = toVisit.head
    val neighbours = map.neighbours(next.pos)
       .filterNot(map.isOutOfBounds)
       .filterNot(next.currentPath.contains)
       .filterNot(map(_) == '#')
       .map(next.step)
    findPathNoCheat(map, toVisit.tail ++ neighbours)

@tailrec
def cheatAndGetNeighbours(map: Map2D[Char], cheatRule: Int, toVisit: List[(Int, Pos)], visited: Set[Pos] = Set(), result: List[(Int, Pos)] = List()): List[(Int, Pos)] =
  if toVisit.isEmpty then result
  else if visited.contains(toVisit.head._2) then
    cheatAndGetNeighbours(map, cheatRule, toVisit.tail, visited, result)
  else if toVisit.head._1 > cheatRule then
    cheatAndGetNeighbours(map, cheatRule, toVisit.tail, visited, result)
  //else if map(toVisit.head._2) == '.' || map(toVisit.head._2) == 'E' then
    //println(s"found cheat path with length: ${toVisit.head._1} ending at ${toVisit.head._2}")
  //  cheatAndGetNeighbours(map, cheatRule, toVisit.tail, visited + toVisit.head._2, toVisit.head :: result)
  else
    val next = toVisit.head._2
    val updatedResult = if map(next) == '.' || map(next) == 'E' then toVisit.head :: result else result
    val neighbours = map.neighbours(next)
      .filterNot(visited.contains)
      .map(p => (toVisit.head._1 + 1, p))
    cheatAndGetNeighbours(map, cheatRule, toVisit.tail ++ neighbours, visited + next, updatedResult)

def solve(map: Map2D[Char], cheatRule: Int): Int =
  val startPos = map.map.find(_._2 == 'S').get._1
  val endPos = map.map.find(_._2 == 'E').get._1

  val pathNoCheating = findPathNoCheat(map, Set(State(startPos))).reverse
  val stepsToEndMap = pathNoCheating.foldRight((0, Map[Pos, Int]()))((pos, acc) => (acc._1 + 1, acc._2 + (pos -> acc._1)))._2.updated(endPos, 0)
  val noCheatingTime = pathNoCheating.size
  val limit = 100
  val bar = pathNoCheating.zipWithIndex.flatMap((pos, steps) =>
    if steps + limit > noCheatingTime then Nil
    else
      val cheatNeighbours = cheatAndGetNeighbours(map, cheatRule, List((0, pos)))
      val foo = cheatNeighbours
        .map(cheat =>
          (steps + cheat._1 + stepsToEndMap(cheat._2), cheat._2)
        )
      if cheatNeighbours.map(_._2).toSet.size != foo.size then throw new RuntimeException(s"Duplicate positions, at $pos")
      val foo2 =  foo.filter(_._1 <= noCheatingTime - limit)
      foo2
  )
  bar.size


@main
def main(): Unit =
  val input = Source.fromResource("day20.txt").getLines().map(_.trim).filterNot(_.isBlank).toList
  val map = parseMap(input, identity, '-')
  println(s"pt1: ${solve(map, 2)}")
  println(s"pt2: ${solve(map, 20)}")

// 216976 - too low
// 241150 - too low
val example =
  """
    |###############
    |#...#...#.....#
    |#.#.#.#.#.###.#
    |#S#...#.#.#...#
    |#######.#.#.###
    |#######.#.#...#
    |#######.#.###.#
    |###..E#...#...#
    |###.#######.###
    |#...###...#...#
    |#.#####.#.###.#
    |#.#...#.#.#...#
    |#.#.#.#.#.#.###
    |#...#...#...###
    |###############
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList