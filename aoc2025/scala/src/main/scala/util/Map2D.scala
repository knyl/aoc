package util

import scala.math.Numeric.Implicits.infixNumericOps

case class Pos(x: Int, y: Int)

case class Map2D[V](map: Map[Pos, V], default: V, width: Int, height: Int):
  def this(map: Map[Pos, V], default: V) = this(map, default, map.maxBy(_._1.x)._1.x, map.maxBy(_._1.y)._1.y)

  def apply(pos: Pos): V = map.getOrElse(pos, default)

  def update(pos: Pos, v: V): Map2D[V] = Map2D(map.updated(pos, v), default, width, height)

  def allKeys: Set[Pos] =
    (for (x <- 0 until width; y <- 0 until height) yield Pos(x, y)).toSet

  def isOutOfBounds(pos: Pos): Boolean =
    pos.x < 0 || pos.x >= width || pos.y < 0 || pos.y >= height


def printMap[V](tiles: Map[Pos, V], xMax: Option[Int] = Option.empty, yMax: Option[Int] = Option.empty): Unit =
  val xs = tiles.keys.map(_.x)
  val ys = tiles.keys.map(_.y)
  val minX = 0//xs.min
  val maxX = xMax.getOrElse(xs.max)
  val minY = 0//ys.min
  val maxY = yMax.getOrElse(ys.max)
  for y <- minY to maxY do
    for x <- minX to maxX do
      print(tiles.getOrElse(Pos(x, y), '.'))
    println()

def parseMap[V](lines: List[String], parseFun: Char => V, default: V): Map2D[V] =
  val map = lines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.map { (c, x) =>
      Pos(x, y) -> parseFun(c)
    }
  }.toMap.filter(_._2 != default)
  val width = lines.headOption.map(_.length).getOrElse(0)
  val height = lines.length
  Map2D(map, default, width, height)

def manhattan_distance[T](p1: (T, T), p2: (T, T))(implicit num: Numeric[T]): T =
  (p1._1 - p2._1).abs + (p1._2 - p2._2).abs
