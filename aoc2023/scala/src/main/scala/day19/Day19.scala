package day19


import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

case class Workflow(label: String, rules: List[Rule])

sealed trait Rule:
  def action: RuleAction

case class LessThan(field: String, value: Int, action: RuleAction) extends Rule

case class GreaterThan(field: String, value: Int, action: RuleAction) extends Rule

case class AllAccept(action: RuleAction) extends Rule

sealed trait RuleAction

case class Accept(label: String) extends RuleAction

case class Reject(label: String) extends RuleAction

case class Next(label: String) extends RuleAction

case class Part(x: Int, m: Int, a: Int, s: Int):
  def sum: Int = x + m + a + s

def pt1(lines: List[String]): Int =
  val workflows = lines.takeWhile(!_.isBlank).map(parseWorkflow).map(w => w.label -> w).toMap
  val parts = lines.dropWhile(!_.isBlank).tail.map(parsePart)
  parts.filter(process(_, "in", workflows)).map(_.sum).sum

def pt2(lines: List[String]): BigInt =
  val workflows = lines.takeWhile(!_.isBlank).map(parseWorkflow).map(w => w.label -> w).toMap
  val start = (1, 4000)
  val tree = processTree(Ranges(start, start, start, start), workflows("in").rules, workflows)
  tree

case class Ranges(x: (Int, Int), m: (Int, Int), a: (Int, Int), s: (Int, Int)):
  def sum: BigInt =
    BigInt(x._2 - x._1 + 1) *
    BigInt(a._2 - a._1 + 1) *
    BigInt(m._2 - m._1 + 1) *
    BigInt(s._2 - s._1 + 1)

def updateMin(str: String, v: Int, values: Ranges): Ranges = str match
  case "x" => values.copy(x = (v, values.x._2))
  case "a" => values.copy(a = (v, values.a._2))
  case "m" => values.copy(m = (v, values.m._2))
  case "s" => values.copy(s = (v, values.s._2))
  case _ => throw new RuntimeException(s"Cannot update $str")

def updateMax(str: String, v: Int, values: Ranges): Ranges = str match
  case "x" => values.copy(x = (values.x._1, v))
  case "a" => values.copy(a = (values.a._1, v))
  case "m" => values.copy(m = (values.m._1, v))
  case "s" => values.copy(s = (values.s._1, v))
  case _ => throw new RuntimeException(s"Cannot update $str")

def processTree(values: Ranges, rules: List[Rule], workflows: Map[String, Workflow]): BigInt = rules.head match
  case GreaterThan(field, value, Accept(_)) =>
    val ok = updateMin(field, value+1, values).sum
    val noValue = updateMax(field, value, values)
    val no = processTree(noValue, rules.tail, workflows)
    ok + no
  case GreaterThan(field, value, Reject(_)) =>
    val noValue = updateMax(field, value, values)
    val no = processTree(noValue, rules.tail, workflows)
    no
  case GreaterThan(field, value, Next(label)) =>
    val okValue = updateMin(field, value+1, values)
    val ok = processTree(okValue, workflows(label).rules, workflows)
    val noValue = updateMax(field, value, values)
    val no = processTree(noValue, rules.tail, workflows)
    ok + no
  case LessThan(field, value, Accept(_)) =>
    val ok = updateMax(field, value-1, values).sum
    val noValue = updateMin(field, value, values)
    val no = processTree(noValue, rules.tail, workflows)
    ok + no
  case LessThan(field, value, Reject(_)) =>
    val noValue = updateMin(field, value, values)
    val no = processTree(noValue, rules.tail, workflows)
    no
  case LessThan(field, value, Next(label)) =>
    val okValue = updateMax(field, value-1, values)
    val ok = processTree(okValue, workflows(label).rules, workflows)
    val noValue = updateMin(field, value, values)
    val no = processTree(noValue, rules.tail, workflows)
    ok + no
  case AllAccept(Accept(_)) => values.sum
  case AllAccept(Reject(_)) => 0
  case AllAccept(Next(label)) => processTree(values, workflows(label).rules, workflows)
  case _ => throw new RuntimeException(s"Cannot process ${rules.head}")

@tailrec
def process(part: Part, workflowLabel: String, workflows: Map[String, Workflow]): Boolean =
  processPart(part, workflows(workflowLabel)) match
    case Accept(_) => true
    case Reject(_) => false
    case Next(label) => process(part, label, workflows)

def processPart(part: Part, workflow: Workflow): RuleAction =
  workflow.rules.find(rule => processRule(part, rule)) match
    case Some(rule) => rule.action
    case None => throw new RuntimeException(s"Cannot find rule for $part")

def processRule(part: Part, condition: Rule): Boolean = condition match
  case GreaterThan(field, value, _) => getField(field, part) > value
  case LessThan(field, value, _) => getField(field, part) < value
  case AllAccept(_) => true

def getField(str: String, part: Part): Int = str match
  case "x" => part.x
  case "m" => part.m
  case "a" => part.a
  case "s" => part.s
  case _ => throw new RuntimeException(s"Cannot get field $str from $part")

def parseWorkflow(line: String): Workflow =
  line match
    case s"$label{$ruleString}" =>
      val rules = ruleString.split(",").map(_.trim).filter(!_.isBlank).toList.map(parseRule)
      Workflow(label, rules)
    case _ => throw new RuntimeException(s"Cannot parse $line")

def parseRule(ruleString: String): Rule =
  ruleString match
    case s"$field<$value:$actionString" => LessThan(field, value.toInt, parseAction(actionString))
    case s"$field>$value:$actionString" => GreaterThan(field, value.toInt, parseAction(actionString))
    case s"$default" => AllAccept(parseAction(default))

def parseAction(action: String): RuleAction = action match
  case s"R" => Reject(action)
  case s"A" => Accept(action)
  case s"$label" => Next(label)
  case _ => throw new RuntimeException(s"Cannot parse $action")

def parsePart(line: String): Part = line match
  case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)
  case _ => throw new RuntimeException(s"Cannot parse $line")

@main
def main(): Unit =
  val lines = Source.fromResource("day19.txt").getLines().map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
