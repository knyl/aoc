package day05

import scala.annotation.tailrec
import scala.io.Source

type Maps = List[List[(Long, Long, Long)]]

def parseToMap(lines: List[String]): (List[Long], Maps) =
  val seeds = lines.head.split(" ").drop(1).map(_.toLong).toList
  (seeds, getMaps(lines.tail))

def getMaps(lines: List[String]): Maps =
  val seedToSoil = createMap(lines.takeWhile(!_.startsWith("soil-to-fertilizer")).drop(1))
  val soilToFertilizer = createMap(lines.dropWhile(!_.startsWith("soil-to-fertilizer")).takeWhile(!_.startsWith("fertilizer-to-water")).drop(1))
  val fertilizerToWater = createMap(lines.dropWhile(!_.startsWith("fertilizer-to-water")).takeWhile(!_.startsWith("water-to-light")).drop(1))
  val waterToLight = createMap(lines.dropWhile(!_.startsWith("water-to-light")).takeWhile(!_.startsWith("light-to-temperature")).drop(1))
  val lightToTemperature = createMap(lines.dropWhile(!_.startsWith("light-to-temperature")).takeWhile(!_.startsWith("temperature-to-humidity")).drop(1))
  val temperatureToHumidity = createMap(lines.dropWhile(!_.startsWith("temperature-to-humidity")).takeWhile(!_.startsWith("humidity-to-location")).drop(1))
  val humidityToLocation = createMap(lines.dropWhile(!_.startsWith("humidity-to-location")).drop(1))
  seedToSoil :: soilToFertilizer :: fertilizerToWater :: waterToLight :: lightToTemperature :: temperatureToHumidity :: humidityToLocation :: Nil

def createMap(lines: List[String]): List[(Long, Long, Long)] =
  lines.map(getRange)

def getRange(line: String): (Long, Long, Long) = line match
  case s"$to $from $length" => (from.toLong, to.toLong, length.toLong)
  case _ => throw new Exception(s"Could not parse range: $line")

def pt1(lines: List[String]): Long =
  val (seeds, maps) = parseToMap(lines)
  seeds.map(getLocation(_, maps)).min

def getLocation(seed: Long, maps: Maps): Long =
  maps.foldLeft(seed)((l, m) => getValue(l, m))

def getValue(seed: Long, value: List[(Long, Long, Long)]) =
  value.find(v => v._1 <= seed && seed <= (v._1 + v._3))
    .map(v => v._2 - v._1 + seed)
    .getOrElse(seed)

def pt2(lines: List[String]): Long =
  val (seedsUngrouped, maps) = parseToMap(lines)
  val seeds = seedsUngrouped.grouped(2).map(l => (l.head, l.last)).toList
  getLowestLocation(0, seeds, maps)

@tailrec
def getLowestLocation(currentLocation: Long, seeds: List[(Long, Long)], maps: Maps): Long =
  val seed = getSeedFromLocation(currentLocation, maps)
  if currentLocation == Long.MaxValue then
    throw new Exception("No solution found")
  else if isInRange(seed, seeds) then
    currentLocation
  else
    getLowestLocation(currentLocation + 1, seeds, maps)

def isInRange(l: Long, seeds: List[(Long, Long)]) =
  seeds.exists(s => s._1 <= l && l <= (s._1 + s._2 - 1))

def getSeedFromLocation(location: Long, maps: Maps): Long =
  maps.foldRight(location)((m, l) => getSeedValue(l, m))

def getSeedValue(l: Long, value: List[(Long, Long, Long)]) =
  value.find(r => r._2 <= l && l <= (r._2 + r._3))
    .map(r => r._1 - r._2 + l)
    .getOrElse(l)

@main
def main(): Unit =
  val lines = Source.fromResource("day05.txt").getLines().filter(!_.isBlank).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))