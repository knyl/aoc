package day12

import scala.io.Source

type Condition = (String, List[Int])
type State = Map[(Int, Int, Int), Long]

def conditionRecords(lines: List[String]): Long =
  lines.map(parseInput).map(calculateCondition).sum

def conditionRecordsUnfolded(lines: List[String]): Long =
  lines.map(parseInput).map(unfold).map(calculateCondition).sum

def unfold(condition: Condition): Condition =
  val (cond, dmg) = condition
  (cond.appended('?').repeat(5).dropRight(1), LazyList.continually(dmg).flatten.take(5 * dmg.length).toList)

def calculateCondition(conditionRecord: Condition): Long =
  val (condition, damage) = conditionRecord
  findMatches(condition, 0, damage, 0, Map()).maxBy(_._2)._2

def findMatches(str: String, pos: Int, damage: List[Int], matchInGroup: Int, state: State): State =
  val dpKey = (pos, matchInGroup, damage.length)
  if state.contains(dpKey) then state
  else
    damage.headOption match
      case None if str.drop(pos).count(_ == '#') == 0 => state + (dpKey -> 1)
      case None => state + (dpKey -> 0)
      case Some(num) if str.length <= pos && num == matchInGroup && damage.length == 1 => state + (dpKey -> 1)
      case Some(_) if str.length <= pos => state + (dpKey -> 0)
      case Some(num) => str(pos) match
        case '.' if matchInGroup == 0 =>
          val state1 = findMatches(str, pos + 1, damage, 0, state)
          state1 + (dpKey -> state1((pos + 1, 0, damage.length)))
        case '.' | '?' if matchInGroup == num =>
          val state1 = findMatches(str, pos + 1, damage.tail, 0, state)
          state1 + (dpKey -> state1((pos + 1, 0, damage.tail.length)))
        case '.' => state + (dpKey -> 0)
        case '?' if matchInGroup == 0 =>
          val state1 = findMatches(str, pos + 1, damage, 1, state)
          val state2 = findMatches(str, pos + 1, damage, 0, state1)
          val matches = state2((pos + 1, 0, damage.length)) + state2((pos + 1, 1, damage.length))
          state2 + (dpKey -> matches)
        case '#' if matchInGroup == num => state + (dpKey -> 0)
        case '?' | '#'=>
          val state1 = findMatches(str, pos + 1, damage, matchInGroup + 1, state)
          state1 + (dpKey -> state1((pos + 1, matchInGroup + 1, damage.length)))
        case _ => throw RuntimeException(s"Invalid state: $str $pos $damage $matchInGroup")

def parseInput(line: String): Condition = line match
  case s"$condition $numbers" => (condition, numbers.split(",").map(_.toInt).toList)
  case _ => throw RuntimeException(s"Invalid input: $line")

@main
def main(): Unit =
  val lines = Source.fromResource("day12.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + conditionRecords(lines))
  println("Pt2: " + conditionRecordsUnfolded(lines))
