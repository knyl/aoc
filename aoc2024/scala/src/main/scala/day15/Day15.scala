package day15

import day15.Direction.{East, West}
import util.{Map2D, Pos, parseMap, printMap}

import scala.annotation.tailrec
import scala.io.Source

enum Direction:
  case North, East, South, West

extension (pos: Pos)
  def walkStep(direction: Direction): Pos = direction match
    case Direction.North => Pos(pos.x, pos.y - 1)
    case Direction.East => Pos(pos.x + 1, pos.y)
    case Direction.South => Pos(pos.x, pos.y + 1)
    case Direction.West => Pos(pos.x - 1, pos.y)

def pt1(map: Map2D[Char], moves: Seq[Direction]): Int =
  val robot = map.map.find(_._2 == '@').get._1
  val finalMap = move(map, robot, moves)
  //printMap(finalMap.map)
  finalMap.map.filter(_._2 == 'O').keys.map(p => p.x + p.y * 100).sum

@tailrec
def move(map: Map2D[Char], robot: Pos, moves: Seq[Direction]): Map2D[Char] =
  if moves.isEmpty then map
  else
    val possibleMove = canMove(map, robot, moves.head)
    if possibleMove.isEmpty then move(map, robot, moves.tail)
    else
      val nextPos = robot.walkStep(moves.head)
      val nextMap = map.update(robot, '.').update(possibleMove.get, 'O').update(nextPos, '@')
      move(nextMap, nextPos, moves.tail)

@tailrec
def canMove(map: Map2D[Char], robot: Pos, direction: Direction): Option[Pos] =
  val nextPos = robot.walkStep(direction)
  if map.isOutOfBounds(nextPos) || map(nextPos) == '#' then Option.empty
  else
    map(nextPos) match
      case '.' => Option(nextPos)
      case 'O' => canMove(map, nextPos, direction)
      case s => throw new Exception(s"Unexpected character: $s at $nextPos")

def parseInput(mapStr: String, line: Seq[String]): (Map2D[Char], Seq[Direction]) =
  val mapString = mapStr.split("\n").filterNot(_.isBlank).toList
  val map = parseMap(mapString, identity, '.')
  val moves = line.head.flatMap {
    case '^' => List(Direction.North)
    case 'v' => List(Direction.South)
    case '<' => List(Direction.West)
    case '>' => List(Direction.East)
    case '\n' => Nil
    case c => throw new Exception(s"Unexpected character: $c")
  }
  (map, moves)

def pt2(map: Map2D[Char], moves: Seq[Direction]): Int =
  val robot = map.map.find(_._2 == '@').get._1
  val finalMap = move2(map, robot, moves)
  printMap(finalMap.map)
  finalMap.map.filter(_._2 == '[').keys.map(p => p.x + p.y * 100).sum

@tailrec
def move2(map: Map2D[Char], robot: Pos, moves: Seq[Direction]): Map2D[Char] =
  if moves.isEmpty then map
  else
    val nextMove = moves.head
    val possibleMove = canMove2(map, robot, nextMove)
    if !possibleMove then move2(map, robot, moves.tail)
    else
      val nextPos = robot.walkStep(nextMove)
      val updatedBoxes = updateBoxes(map, robot, nextMove)
      val updatedBoxesCorrect = updatedBoxes.groupBy(_._1).view.mapValues(v =>
        if v.size >= 2 && v.map(_._2).forall(_ == '.') then v.head._2
        else if v.size >= 2 then v.map(_._2).filterNot(_ == '.').head
        else v.head._2
      ).toList
      val nextMap = updatedBoxesCorrect.foldLeft(map)((map, box) => map.update(box._1, box._2)).update(robot, '.').update(nextPos, '@')
      move2(nextMap, nextPos, moves.tail)

def updateBoxes(map: Map2D[Char], pos: Pos, direction: Direction): List[(Pos, Char)] = direction match
  case Direction.North | Direction.South => updateBoxesVertically(map, pos, direction)
  case Direction.East | Direction.West => updateBoxesHorizontally(map, pos, direction)

def updateBoxesVertically(map: Map2D[Char], pos: Pos, direction: Direction): List[(Pos, Char)] =
  val nextPos = pos.walkStep(direction)
  if (map(pos) == '[' || map(pos) == '@') && map(nextPos) == ']' then
    (nextPos, map(pos)) :: (nextPos.walkStep(West), '.') ::
      updateBoxesVertically(map, nextPos, direction) ++
        updateBoxesVertically(map, nextPos.walkStep(West), direction)
  else if (map(pos) == ']' || map(pos) == '@') && map(nextPos) == '[' then
    (nextPos, map(pos)) :: (nextPos.walkStep(East), '.') ::
      updateBoxesVertically(map, nextPos, direction) ++
        updateBoxesVertically(map, nextPos.walkStep(East), direction)
  else if (map(pos) == '[' && map(nextPos) == '[') || (map(pos) == ']' && map(nextPos) == ']') then
    (nextPos, map(pos)) :: updateBoxes(map, nextPos, direction)
  else if map(nextPos) == '.' then
    (nextPos, map(pos)) :: Nil
  else
    printMap(map.map)
    throw new Exception(s"Unexpected character: ${map(nextPos)} at $nextPos")

def updateBoxesHorizontally(map: Map2D[Char], pos: Pos, direction: Direction): List[(Pos, Char)] =
  val nextPos = pos.walkStep(direction)
  map(nextPos) match
    case '.' => (nextPos, map(pos)) :: Nil
    case '[' | ']' => (nextPos, map(pos)) :: updateBoxesHorizontally(map, nextPos, direction)
    case s => throw new Exception(s"Unexpected character: $s at $nextPos")

def canMove2(map: Map2D[Char], robot: Pos, direction: Direction): Boolean = direction match
  case Direction.North | Direction.South => canMoveVertically(map, robot, direction)
  case Direction.East | Direction.West => canMoveHorizontally(map, robot, direction)

def canMoveVertically(map: Map2D[Char], pos: Pos, direction: Direction): Boolean =
  val nextPos = pos.walkStep(direction)
  if map.isOutOfBounds(nextPos) || map(nextPos) == '#' then false
  else
    map(nextPos) match
      case '.' => true
      case '[' => canMoveVertically(map, nextPos, direction) && canMoveVertically(map, nextPos.walkStep(Direction.East), direction)
      case ']' => canMoveVertically(map, nextPos, direction) && canMoveVertically(map, nextPos.walkStep(Direction.West), direction)
      case s => throw new Exception(s"Unexpected character: $s at $nextPos")

@tailrec
def canMoveHorizontally(map: Map2D[Char], pos: Pos, direction: Direction): Boolean =
  val nextPos = pos.walkStep(direction)
  if map.isOutOfBounds(nextPos) || map(nextPos) == '#' then false
  else
    map(nextPos) match
      case '.' => true
      case '[' | ']' => canMoveHorizontally(map, nextPos, direction)
      case s => throw new Exception(s"Unexpected character: $s at $nextPos")

@main
def main(): Unit =
  val input = Source.fromResource("day15.txt").mkString.split("\n\n").map(_.trim).filterNot(_.isBlank).toList
  val (map, moves) = parseInput(input.head, input.tail)
  println(s"pt1: ${pt1(map, moves)}")

  val dataPt2 = input
  val biggerMap = dataPt2.head.flatMap(c => c match
    case 'O' => List('[', ']')
    case '#' => List('#', '#')
    case '.' => List('.', '.')
    case '@' => List('@', '.')
    case '\n' => List('\n')
  ).mkString
  val (map2, moves2) = parseInput(biggerMap, dataPt2.tail)
  println(s"pt2: ${pt2(map2, moves2)}")
