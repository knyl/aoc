package util

type Pos = (Int, Int)

class Map2D[V](val map: Map[Pos, V], val default: V):
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
  }.toMap
  Map2D(map, default)
