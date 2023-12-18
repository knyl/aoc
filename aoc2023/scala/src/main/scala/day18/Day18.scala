package day18

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

sealed trait Instruction:
  def n: Int

case class Right(n: Int) extends Instruction

case class Down(n: Int) extends Instruction

case class Left(n: Int) extends Instruction

case class Up(n: Int) extends Instruction

case class Pos(x: Int, y: Int) extends Ordered[Pos]:
  def compare(that: Pos): Int = this.x compare that.x match
    case 0 => this.y compare that.y
    case x => x

def pt1(lines: List[String]): Int =
  val instructions = parseInput(lines)
  val path = createPath(instructions)
  val corner = path.toList.min
  val startCorner = Pos(corner.x + 1, corner.y + 1)
  val allInside = bfs(Set(startCorner), Set(), path)
  allInside.size + path.size

def pt2(lines: List[String]): BigInt =
  val instructions = parseInput2(lines)
  val boundary = instructions.map(_.n).sum
  val path = createPath2(instructions)
  val area = shoelace(path)
  BigInt(boundary / 2) + area + 1

def shoelace(path: List[Pos]): BigInt =
  val xs = path.map(_.x).map(BigInt.apply)
  val ys = path.map(_.y).map(BigInt.apply)
  val additions = ((xs.last, ys.head) :: (xs zip ys.tail)).map((x, y) => x * y)
  val subtractions = ((xs.head, ys.last) :: (xs.tail zip ys)).map((x, y) => x * y)
  val result = (additions zip subtractions).map((x, y) => x - y).sum
  result.abs / 2

def isOutside(minX: Int, maxX: Int, minY: Int, maxY: Int)(pos: Pos): Boolean =
  pos.x < minX || pos.y < minY || pos.x > maxX || pos.y > maxY

@tailrec
def createPath(instructions: List[Instruction], currPos: Pos = Pos(0, 0), path: Set[Pos] = Set()): Set[Pos] = instructions.headOption match
  case None => path
  case Some(instruction) =>
    val newPathSegment = createPathSegment(currPos, instruction)
    createPath(instructions.tail, newPathSegment.last, path ++ newPathSegment)

@tailrec
def createPath2(instructions: List[Instruction], currPos: Pos = Pos(0, 0), path: List[Pos] = List()): List[Pos] = instructions.headOption match
  case None => path
  case Some(instruction) =>
    val newPathSegment = createPathSegment(currPos, instruction).last
    createPath2(instructions.tail, newPathSegment, newPathSegment :: path)

def createPathSegment(pos: Pos, instruction: Instruction): List[Pos] = instruction match
  case Right(n) => (pos.x to pos.x + n).map(x => Pos(x, pos.y)).toList
  case Down(n) => (pos.y to pos.y + n).map(y => Pos(pos.x, y)).toList
  case Left(n) => (pos.x to pos.x - n by -1).map(x => Pos(x, pos.y)).toList
  case Up(n) => (pos.y to pos.y - n by -1).map(y => Pos(pos.x, y)).toList

@tailrec
def bfs(toVisit: Set[Pos], visited: Set[Pos], path: Set[Pos]): Set[Pos] = toVisit.headOption match
  case None => visited
  case Some(pos) =>
    val newToVisit = neighbours(pos).diff(path).diff(visited)
    bfs(toVisit.tail ++ newToVisit, visited + pos, path)

def neighbours(p: Pos): Set[Pos] =
  Set(Pos(p.x + 1, p.y), Pos(p.x - 1, p.y), Pos(p.x, p.y + 1), Pos(p.x, p.y - 1))

def parseInput(lines: List[String]): List[Instruction] =
  lines.map(parseInstruction)

def parseInstruction(line: String): Instruction = line match
  case s"R $n ($_)" => Right(n.toInt)
  case s"L $n ($_)" => Left(n.toInt)
  case s"U $n ($_)" => Up(n.toInt)
  case s"D $n ($_)" => Down(n.toInt)

def parseInput2(lines: List[String]): List[Instruction] =
  lines.map(parseInstruction2)

def parseInstruction2(line: String): Instruction = line match
  case s"$_ $_ (#$instr)" => toInstruction(instr.dropRight(1), instr.last)

def toInstruction(distance: String, direction: Char): Instruction = direction match
  case '0' => Right(Integer.parseInt(distance, 16))
  case '1' => Down(Integer.parseInt(distance, 16))
  case '2' => Left(Integer.parseInt(distance, 16))
  case '3' => Up(Integer.parseInt(distance, 16))

@main
def main(): Unit =
  val lines = Source.fromResource("day18.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
