package day24

import scala.annotation.targetName
import scala.io.Source
import scala.language.postfixOps

case class Vec(x: BigDecimal, y: BigDecimal, z: BigDecimal):
  def cross(other: Vec): Vec =
    val p2x = this.y * other.z - this.z * other.y
    val p2y = this.z * other.x - this.x * other.z
    val p2z = this.x * other.y - this.y * other.x
    Vec(p2x, p2y, p2z)
  def dot(other: Vec): BigDecimal =
    this.x * other.x + this.y * other.y + this.z * other.z
  @targetName("addVec")
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y, this.z + other.z)
  @targetName("subVec")
  def -(other: Vec): Vec = Vec(this.x - other.x, this.y - other.y, this.z - other.z)
  @targetName("scalarMulVec")
  def *(scalar: BigDecimal): Vec = Vec(this.x * scalar, this.y * scalar, this.z * scalar)
  @targetName("divVec")
  def /(scalar: BigDecimal): Vec = Vec(this.x / scalar, this.y / scalar, this.z / scalar)

case class Stone(p: Vec, v: Vec):
  def this(x: BigDecimal, y: BigDecimal, z: BigDecimal, dx: BigDecimal, dy: BigDecimal, dz: BigDecimal) = this(Vec(x, y, z), Vec(dx, dy, dz))
  private def hasPassedX(x: BigDecimal): Boolean =
    if this.v.x > 0 then this.p.x > x
    else this.p.x < x
  private def hasPassedY(y: BigDecimal): Boolean =
    if this.v.y > 0 then this.p.y > y
    else this.p.y < y
  def intersectXY(other: Stone): Option[(BigDecimal, BigDecimal)] =
    val x1 = p.x + v.x
    val y1 = p.y + v.y
    val m1 = (y1 - p.y) / (x1 - p.x)
    val x2 = other.p.x + other.v.x
    val y2 = other.p.y + other.v.y
    val m2 = (y2 - other.p.y) / (x2 - other.p.x)
    if m1 == m2 then None
    else
      val b1 = y1 - m1 * x1
      val b2 = y2 - m2 * x2
      val intersectX = (b2 - b1) / (m1 - m2)
      val intersectY = m1 * intersectX + b1
      val intersectionInThePast = hasPassedX(intersectX) || other.hasPassedX(intersectX) || hasPassedY(intersectY) || other.hasPassedY(intersectY)
      if intersectionInThePast then None
      else Some((intersectX, intersectY))

def pt1(lines: List[String]): Int =
  val points = lines.map(parseLine)
  val intersections = points.combinations(2)
    .map(pair => pair.head.intersectXY(pair.last)).toList
  intersections.filter(_.isDefined).map(_.get).count(isInRange)

def isInRange(intersect: (BigDecimal, BigDecimal)): Boolean =
  val (x, y) = intersect
  x >= BigDecimal("200000000000000") && y >= BigDecimal("200000000000000") && x <= BigDecimal("400000000000000") && y <= BigDecimal("400000000000000")

def pt2(lines: List[String]): BigDecimal =
  val points = lines.map(parseLine)
  val point0 = points.head
  val point1 = points.tail.head
  val point2 = points.tail.tail.head

  // Everything is relative to the first point (point0)
  val p1 = point1.p - point0.p
  val v1 = point1.v - point0.v
  val p2 = point2.p - point0.p
  val v2 = point2.v - point0.v

  // The collisions are at: (viewed from point0)
  // p1 + t1 * v1
  // p2 + t2 * v2

  // We know p1, v1, p2, v2
  // Linear algebra magic by someone else to get equations for t1 and t2
  // which is time for the collision for p1 and p2 with the thrown stone
  val t1 = -(p1.cross(p2).dot(v2) / v1.cross(p2).dot(v2))
  val t2 = -(p1.cross(p2).dot(v1) / p1.cross(v2).dot(v1))

  // Position of the collision between the thrown stone and p1 and p2
  val c1 = p1 + v1 * t1
  val c2 = p2 + v2 * t2

  // The velocity of the thrown stone
  val v = (c2 - c1) / (t2 - t1)
  // The starting position
  val p = c1 - v * t1
  // Since we are relative to point0, we need to add the position of point0
  val thrownPos = point0.p + p
  thrownPos.x + thrownPos.y + thrownPos.z


@main
def main(): Unit =
  val lines = Source.fromResource("day24.txt").getLines().map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))

def parseLine(line: String): Stone = line match
  case s"$x, $y, $z @ $dx, $dy, $dz" => Stone(Vec(parseNum(x), parseNum(y), parseNum(z)), Vec(parseNum(dx), parseNum(dy), parseNum(dz)))
  case _ => throw RuntimeException(s"Cannot parse line: $line")

def parseNum(s: String): BigDecimal = BigDecimal(s.trim)

val example =
  """
    |19, 13, 30 @ -2,  1, -2
    |18, 19, 22 @ -1, -1, -2
    |20, 25, 34 @ -2, -2, -4
    |12, 31, 28 @ -1, -2, -1
    |20, 19, 15 @  1, -5, -3
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList
