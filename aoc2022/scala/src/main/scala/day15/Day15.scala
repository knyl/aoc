package day15

import scala.io.Source

case class Position(x: Int, y: Int)

case class Sensor(sensor: Position, beacon: Position, distance: Int)

def manhattanDistance(p1: Position, p2: Position): Int =
  Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

def parseInput(string: String) = string match
  case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
    val sensor = Position(sx.toInt, sy.toInt)
    val beacon = Position(bx.toInt, by.toInt)
    Sensor(sensor, beacon, manhattanDistance(sensor, beacon))
  case _ => throw new RuntimeException(s"Invalid input: $string")

def pt1(sensors: List[Sensor]) =
  //val row = 10
  val row = 2000000
  val sensorsCoveringRow = sensors.filter(sensorCoversRow(row))
  val coveredPositions = sensorsCoveringRow.foldLeft(Set())((acc: Set[Position], sensor: Sensor) => acc.union(positionsOnRow(row, sensor)))
  coveredPositions.size - 1

def sensorCoversRow(row: Int)(s: Sensor) =
  s.sensor.y == row || (s.sensor.y < row && s.sensor.y + s.distance >= row) || (s.sensor.y > row && s.sensor.y - s.distance <= row)

def positionsOnRow(row: Int, s: Sensor) =
  val yDistance = Math.abs(s.sensor.y - row)
  val xToCover = s.distance - yDistance
  val start = s.sensor.x - xToCover
  val end = s.sensor.x + xToCover
  (start to end).map(x => Position(x, row)).toSet

@main
def main(): Unit =
  val input = Source.fromResource("day15.txt").getLines().toList
  val sensors = input.map(parseInput)
  println("Part 1: " + pt1(sensors))

val TEST =
  """
    |Sensor at x=2, y=18: closest beacon is at x=-2, y=15
    |Sensor at x=9, y=16: closest beacon is at x=10, y=16
    |Sensor at x=13, y=2: closest beacon is at x=15, y=3
    |Sensor at x=12, y=14: closest beacon is at x=10, y=16
    |Sensor at x=10, y=20: closest beacon is at x=10, y=16
    |Sensor at x=14, y=17: closest beacon is at x=10, y=16
    |Sensor at x=8, y=7: closest beacon is at x=2, y=10
    |Sensor at x=2, y=0: closest beacon is at x=2, y=10
    |Sensor at x=0, y=11: closest beacon is at x=2, y=10
    |Sensor at x=20, y=14: closest beacon is at x=25, y=17
    |Sensor at x=17, y=20: closest beacon is at x=21, y=22
    |Sensor at x=16, y=7: closest beacon is at x=15, y=3
    |Sensor at x=14, y=3: closest beacon is at x=15, y=3
    |Sensor at x=20, y=1: closest beacon is at x=15, y=3
    |""".stripMargin.trim.split("\n").toList