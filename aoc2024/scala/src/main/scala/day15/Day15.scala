package day15

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
  finalMap.map.filter(_._2 == 'O').map(_._1).map(p => p.x + p.y * 100).sum

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

def parseInput(line: Seq[String]): (Map2D[Char], Seq[Direction]) =
  val mapString = line.head.split("\n").filterNot(_.isBlank).toList
  val map = parseMap(mapString, identity, '.')
  val moves = line.last.flatMap {
    case '^' => List(Direction.North)
    case 'v' => List(Direction.South)
    case '<' => List(Direction.West)
    case '>' => List(Direction.East)
    case '\n' => Nil
    case c => throw new Exception(s"Unexpected character: $c")
  }
  (map, moves)

@main
def main(): Unit =
  val input = Source.fromResource("day15.txt").mkString.split("\n\n").map(_.trim).filterNot(_.isBlank).toList
  val (map, moves) = parseInput(input)
  println(s"pt1: ${pt1(map, moves)}")
  println(s"pt2: ${}")

val example =
  """
    |########
    |#..O.O.#
    |##@.O..#
    |#...O..#
    |#.#.O..#
    |#...O..#
    |#......#
    |########
    |
    |<^^>>>vv<v>>v<<
    |""".stripMargin.split("\n\n").map(_.trim).filterNot(_.isBlank).toList

// 10092
val example2 =
  """
    |##########
    |#..O..O.O#
    |#......O.#
    |#.OO..O.O#
    |#..O@..O.#
    |#O#..O...#
    |#O..O..O.#
    |#.OO.O.OO#
    |#....O...#
    |##########
    |
    |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
    |""".stripMargin.split("\n\n").map(_.trim).filterNot(_.isBlank).toList