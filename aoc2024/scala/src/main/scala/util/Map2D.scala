package util

import scala.math.Numeric.Implicits.infixNumericOps

type Pos = (Int, Int)

class Map2D[V](val map: Map[Pos, V], val default: V, val width: Int, val height: Int):
  def this(map: Map[Pos, V], default: V) = this(map, default, map.maxBy(_._1._1)._1._1, map.maxBy(_._1._2)._1._2)
  def apply(pos: Pos): V = map.getOrElse(pos, default)


def printMap[V](tiles: Map[Pos, V]): Unit =
  val xs = tiles.keys.map(_._1)
  val ys = tiles.keys.map(_._2)
  val minX = xs.min
  val maxX = xs.max
  val minY = ys.min
  val maxY = ys.max
  for y <- minY to maxY do
    for x <- minX to maxX do
      print(tiles.getOrElse((x, y), '.'))
    println()

def parseMap[V](lines: List[String], parseFun: Char => V, default: V): Map2D[V] =
  val map = lines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.map { (c, x) =>
      (x, y) -> parseFun(c)
    }
  }.toMap.filter(_._2 != default)
  val width = lines.headOption.map(_.length).getOrElse(0)
  val height = lines.length
  Map2D(map, default, width, height)

def manhattan_distance[T](p1: (T, T), p2: (T, T))(implicit num: Numeric[T]): T =
  (p1._1 - p2._1).abs + (p1._2 - p2._2).abs
