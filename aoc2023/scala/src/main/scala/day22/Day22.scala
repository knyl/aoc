package day22

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.language.postfixOps


case class Pos(x: Int, y: Int, z: Int)

case class Brick(p1: Pos, p2: Pos):
  def overlaps(other: Brick): Boolean =
    overlap((p1.x, p2.x), (other.p1.x, other.p2.x)) && overlap((p1.y, p2.y), (other.p1.y, other.p2.y))

  def fall(zDiff: Int): Brick =
    Brick(p1.copy(z = p1.z - zDiff), p2.copy(z = p2.z - zDiff))

def overlap(x: (Int, Int), y: (Int, Int)): Boolean =
  val (x1, x2) = x
  val (y1, y2) = y
  val xMax = Math.max(x1, x2)
  val xMin = Math.min(x1, x2)
  val yMax = Math.max(y1, y2)
  val yMin = Math.min(y1, y2)
  Math.max(xMin, yMin) <= Math.min(xMax, yMax)

def pt1(lines: List[String]): Int =
  val bricks = lines.map(parseLine).sortBy(b => b.p1.z)
  val settledBricks = fallDown(bricks)
  val singleSupporters = getSingleSupporters(settledBricks)
  settledBricks.count(b => !singleSupporters.contains(b))

def pt2(lines: List[String]): Long =
  val bricks = lines.map(parseLine).sortBy(b => b.p1.z)
  val settledBricks = fallDown(bricks)
  val bricksAbove = getBricksAbove(settledBricks.reverse)
  val singleSupporters = getSingleSupporters(settledBricks)
  val bricksBelow = brickToBricksBelow(settledBricks)
  val brickOrdering: Ordering[Brick] = Ordering.by(b => (b.p1.z, b.p2.z))
  val disintegratedBricks = singleSupporters.map(b => getDisintegratedBricks(mutable.PriorityQueue(b)(brickOrdering), bricksAbove, bricksBelow, Set(b)).toList).toList
  disintegratedBricks.map(_.size).sum - disintegratedBricks.size

@tailrec
def fallDown(bricksInAir: List[Brick], settledBricks: List[Brick] = List()): List[Brick] =
  bricksInAir.headOption match
    case None => settledBricks
    case Some(brick) =>
      fallDown(bricksInAir.tail, moveDown(brick, settledBricks) :: settledBricks)

def moveDown(brick: Brick, settledBricks: List[Brick]): Brick =
  val firstOverlapping = settledBricks.sortBy(_.p2.z).findLast(_.overlaps(brick))
  val newZ = firstOverlapping.map(_.p2.z).getOrElse(0) + 1
  val zDiff = brick.p1.z - newZ
  brick.fall(zDiff)

@tailrec
def getSingleSupporters(bricks: List[Brick], singleSupporters: Set[Brick] = Set()): Set[Brick] =
  bricks.headOption match
    case None => singleSupporters
    case Some(brick) =>
      val bricksBelow = bricks.tail.filter(_.p2.z == brick.p1.z - 1)
      val overlappingBricks = bricksBelow.filter(_.overlaps(brick))
      if overlappingBricks.size == 1 then
        getSingleSupporters(bricks.tail, singleSupporters + overlappingBricks.head)
      else
        getSingleSupporters(bricks.tail, singleSupporters)

@tailrec
def brickToBricksBelow(bricks: List[Brick], singleSupporters: Map[Brick, List[Brick]] = Map()): Map[Brick, List[Brick]] =
  bricks.headOption match
    case None => singleSupporters
    case Some(brick) =>
      val bricksBelow = bricks.tail.filter(_.p2.z == brick.p1.z - 1)
      val overlappingBricks = bricksBelow.filter(_.overlaps(brick))
      if overlappingBricks.nonEmpty then
        brickToBricksBelow(bricks.tail, singleSupporters + (brick -> overlappingBricks))
      else
        brickToBricksBelow(bricks.tail, singleSupporters)

@tailrec
def getBricksAbove(bricks: List[Brick], supporters: Map[Brick, List[Brick]] = Map()): Map[Brick, List[Brick]] =
  bricks.headOption match
    case None => supporters
    case Some(brick) =>
      val bricksAbove = bricks.tail.filter(_.p1.z == brick.p2.z + 1)
      val overlappingBricks = bricksAbove.filter(_.overlaps(brick))
      if overlappingBricks.nonEmpty then
        getBricksAbove(bricks.tail, supporters + (brick -> overlappingBricks))
      else
        getBricksAbove(bricks.tail, supporters)

@tailrec
def getDisintegratedBricks(bricks: mutable.PriorityQueue[Brick],
                           brickToAbove: Map[Brick, List[Brick]],
                           brickToBelow: Map[Brick, List[Brick]],
                           disintegrated: Set[Brick],
                           isFirst: Boolean = true): Set[Brick] =
  if bricks.isEmpty then disintegrated
  else
    val brick = bricks.dequeue()
    val bricksBelow = brickToBelow.getOrElse(brick, List())
    if isFirst || bricksBelow.forall(disintegrated.contains) then
      val bricksAbove = brickToAbove.getOrElse(brick, List())
      getDisintegratedBricks(bricks ++= bricksAbove, brickToAbove, brickToBelow, disintegrated + brick, false)
    else
      getDisintegratedBricks(bricks, brickToAbove, brickToBelow, disintegrated, false)

def parseLine(line: String): Brick = line match
  case s"$x1,$y1,$z1~$x2,$y2,$z2" => Brick(Pos(x1.toInt, y1.toInt, z1.toInt), Pos(x2.toInt, y2.toInt, z2.toInt))
  case _ => throw new Exception(s"Invalid line: $line")

@main
def main(): Unit =
  val lines = Source.fromResource("day22.txt").getLines().map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
