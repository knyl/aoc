package day08

import scala.annotation.tailrec
import scala.io.Source

type Node = String

enum Direction:
  case Left, Right

def pt1(lines: List[String]): Long =
  val (instructions, map) = parseInput(lines)
  followInstructions(LazyList.continually(instructions).flatten, "AAA", map)

def followInstruction(direction: Direction, node: Node, map: Map[Node, (Node, Node)]) = direction match
  case Direction.Left => map(node)._1
  case Direction.Right => map(node)._2

@tailrec
def followInstructions(instructions: LazyList[Direction], node: Node, map: Map[Node, (Node, Node)], count: Long = 0): Long =
  followInstruction(instructions.head, node, map) match
    case "ZZZ" => count + 1
    case _ => followInstructions(instructions.tail, followInstruction(instructions.head, node, map), map, count + 1)

def pt2(lines: List[String]): BigInt =
  val (instructions, map) = parseInput(lines)
  val startingNodes = map.filter(_._1.endsWith("A")).keys.toList
  val stepsToEnds = startingNodes.map(followInstructions2(LazyList.continually(instructions).flatten, _, map))
  val greatestCommonDivisor = gcdOfList(stepsToEnds)
  stepsToEnds.map(_ / greatestCommonDivisor).product * greatestCommonDivisor

@tailrec
def followInstructions2(instructions: LazyList[Direction], node: Node, map: Map[Node, (Node, Node)], count: Long = 0): Long =
  val updatedNode = followInstruction(instructions.head, node, map)
  if updatedNode.endsWith("Z") then count + 1
  else followInstructions2(instructions.tail, followInstruction(instructions.head, node, map), map, count + 1)

def gcdOfList(numbers: List[Long]): BigInt =
  numbers.foldLeft(BigInt(numbers.head))((acc: BigInt, n: Long) => gcd(acc, BigInt(n)))

@tailrec
def gcd(a: BigInt, b: BigInt): BigInt =
  if b == 0 then a.max(-a)
  else gcd(b, a % b)

def parseInput(lines: List[String]): (List[Direction], Map[Node, (Node, Node)]) =
  val instructions = lines.head.toCharArray.map(toDirection).toList
  val map = parseMap(lines.tail)
  (instructions, map)

def toDirection(c: Char): Direction = c match
  case 'L' => Direction.Left
  case 'R' => Direction.Right
  case _ => throw new Exception(s"Invalid direction $c")

def parseMap(lines: List[String]): Map[Node, (Node, Node)] =
  (lines map {
    case s"$origin = ($left, $right)" => (origin, (left, right))
    case line => throw new Exception(s"Invalid line $line")
  }).toMap

@main
def main(): Unit =
  val lines = Source.fromResource("day08.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
