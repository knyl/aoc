package day14


import scala.annotation.tailrec
import scala.io.Source

case class Pos(x: Int, y: Int)

case class Velocity(x: Int, y: Int)

case class Robot(pos: Pos, velocity: Velocity)

def pt1(robots: List[Robot]): Int =
  val time = 100
  val xMax = 101
  val yMax = 103
  val updatedRobots = robots.map(r =>
    val newX = r.pos.x + r.velocity.x * time
    val newY = r.pos.y + r.velocity.y * time
    Pos((newX % xMax + xMax) % xMax, (newY % yMax + yMax) % yMax))
  val q1Robots = updatedRobots.count(r => r.x < xMax / 2 && r.y < yMax / 2)
  val q2Robots = updatedRobots.count(r => r.x > xMax / 2 && r.y < yMax / 2)
  val q3Robots = updatedRobots.count(r => r.x < xMax / 2 && r.y > yMax / 2)
  val q4Robots = updatedRobots.count(r => r.x > xMax / 2 && r.y > yMax / 2)
  q1Robots * q2Robots * q3Robots * q4Robots

def pt2(robots: Seq[Robot]): Unit =
  // Robots have visited each square after 10402 seconds
  // Within that time the christmas tree needs to have appeared
  val xMax = 101
  val yMax = 103
  for time <- 0 to 10403 do
    val robotsAtTime = getRobotsAtTime(robots, time)
    val robotPositions = robotsAtTime.toSet
    val robotsClose = robotPositions.toList.count(r => hasAtLeastOneNeighbour(r, robotPositions))
    if robotsClose > 300 then
      println(s"time: $time")
      printRobots(robotsAtTime, xMax, yMax)


def hasAtLeastOneNeighbour(robot: Pos, robots: Set[Pos]): Boolean =
  val neighbours = getNeighbours(robot)
  neighbours.exists(robots.contains)

def getNeighbours(robot: Pos): Set[Pos] =
  Set(
    Pos(robot.x - 1, robot.y - 1),
    Pos(robot.x - 1, robot.y + 1),
    Pos(robot.x + 1, robot.y + 1),
    Pos(robot.x + 1, robot.y - 1),
    Pos(robot.x, robot.y - 1),
    Pos(robot.x, robot.y + 1),
    Pos(robot.x - 1, robot.y),
    Pos(robot.x + 1, robot.y)
  ).filter(p => p.x >= 0 && p.x < 101 && p.y >= 0 && p.y < 103)

def getRobotsAtTime(robots: Seq[Robot], time: Int): Seq[Pos] =
  robots.map(r =>
    val newX = r.pos.x + r.velocity.x * time
    val newY = r.pos.y + r.velocity.y * time
    Pos((newX % 101 + 101) % 101, (newY % 103 + 103) % 103))

def printRobots(robots: Seq[Pos], xMax: Int, yMax: Int): Unit =
  val positions = robots.groupBy(identity).view.mapValues(_.size.toString)
  for y <- 0 until yMax do
    for x <- 0 until xMax do
      print(positions.getOrElse(Pos(x, y), "."))
    println()
  println(s"--------------------")

@tailrec
def findRecurringPattern(robot: Robot, xMax: Int, yMax: Int, visited: Set[Pos] = Set()): Set[Pos] =
  val newPos = move(robot, xMax, yMax)
  if visited.contains(newPos) then visited
  else findRecurringPattern(Robot(newPos, robot.velocity), xMax, yMax, visited + robot.pos)

def move(robot: Robot, xMax: Int, yMax: Int): Pos =
  val newX = robot.pos.x + robot.velocity.x
  val newY = robot.pos.y + robot.velocity.y
  Pos((newX % xMax + xMax) % xMax, (newY % yMax + yMax) % yMax)

def parseInput(str: String): Robot = str match
  case s"p=$x,$y v=$vx,$vy" => Robot(Pos(x.toInt, y.toInt), Velocity(vx.toInt, vy.toInt))
  case _ => throw IllegalArgumentException(s"Invalid input: $str")

@main
def main(): Unit =
  val input = Source.fromResource("day14.txt").getLines().map(_.trim).toList
  val robots = input.map(parseInput)
  println(s"pt1: ${pt1(robots)}")
  println(s"pt2: ${pt2(robots)}")
