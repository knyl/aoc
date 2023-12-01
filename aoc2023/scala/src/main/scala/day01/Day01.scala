package day01
import scala.annotation.tailrec
import scala.io.Source

def calibrationPt1(line: String): Int =
  val numbers = line.filter(_.isDigit)
  List(numbers.head, numbers.last).mkString.toInt

@tailrec
def getFirst(line: String): Int =
  line match
    case s"0$_" => 0
    case s"1$_" => 1
    case s"2$_" => 2
    case s"3$_" => 3
    case s"4$_" => 4
    case s"5$_" => 5
    case s"6$_" => 6
    case s"7$_" => 7
    case s"8$_" => 8
    case s"9$_" => 9
    case s"one$_" => 1
    case s"two$_" => 2
    case s"three$_" => 3
    case s"four$_" => 4
    case s"five$_" => 5
    case s"six$_" => 6
    case s"seven$_" => 7
    case s"eight$_" => 8
    case s"nine$_" => 9
    case s"$rest" => getFirst(rest.tail)

@tailrec
def getLast(line: String): Int =
  line match
    case s"" => throw new Exception("No last digit")
    case s"0$_" => 0
    case s"1$_" => 1
    case s"2$_" => 2
    case s"3$_" => 3
    case s"4$_" => 4
    case s"5$_" => 5
    case s"6$_" => 6
    case s"7$_" => 7
    case s"8$_" => 8
    case s"9$_" => 9
    case s"eno$_" => 1
    case s"owt$_" => 2
    case s"eerht$_" => 3
    case s"ruof$_" => 4
    case s"evif$_" => 5
    case s"xis$_" => 6
    case s"neves$_" => 7
    case s"thgie$_" => 8
    case s"enin$_" => 9
    case s"$rest" => getLast(rest.tail)

def calibrationPt2(line: String): Int =
  val first = getFirst(line)
  val last = getLast(line.reverse)
  List(first, last).mkString.toInt

@main
def main(): Unit =
  val lines = Source.fromResource("day01.txt").getLines().toList

  println("Pt1: " + lines.map(calibrationPt1).sum)
  println("Pt2: " + lines.map(calibrationPt2).sum)