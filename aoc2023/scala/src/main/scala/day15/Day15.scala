package day15

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

type Lenses = List[(String, Int)]
type Boxes = Map[Int, Lenses]

def pt1(value: String): Int =
  value.split(",").map(calculateHash(_)).sum

def pt2(value: String): Int =
  val instructions = value.split(",").toList
  performInstruction(instructions).map(focusingPower).sum

def focusingPower(box: (Int, Lenses)): Int =
  val (boxNr, lenses) = box
  val boxValue = boxNr + 1
  lenses.reverse.zipWithIndex.map((lens, index) => boxValue * (index + 1) * lens._2).sum

@tailrec
def performInstruction(instructions: List[String], boxes: Boxes = Map()): Boxes = instructions.headOption match
  case None => boxes
  case Some(instruction) => instruction match
    case s"$label=$lens" => performInstruction(instructions.tail, addToBox(label, lens.toInt, boxes))
    case s"$label-" => performInstruction(instructions.tail, removeFromBox(label, boxes))

def addToBox(label: String, lens: Int, boxes: Boxes): Boxes =
  val boxNr = calculateHash(label)
  val box = boxes.getOrElse(boxNr, List())
  if !box.exists(_._1 == label) then boxes + (boxNr -> ((label, lens) :: box))
  else
    val indexOfLabel = box.indexWhere(_._1 == label)
    val updatedBox = box.patch(indexOfLabel, Seq((label, lens)), 1)
    boxes + (boxNr -> updatedBox)

def removeFromBox(label: String, boxes: Boxes): Boxes =
  val boxNr = calculateHash(label)
  val box = boxes.getOrElse(boxNr, List())
  val updatedBox = box.filterNot(_._1 == label)
  boxes + (boxNr -> updatedBox)

@tailrec
def calculateHash(line: String, currentValue: Int = 0): Int = line.headOption match
  case Some(c) => calculateHash(line.tail, (currentValue + c.toInt) * 17 % 256)
  case None => currentValue

@main
def main(): Unit =
  val line = Source.fromResource("day15.txt").getLines().filter(!_.isBlank).toList.head

  println("Pt1: " + pt1(line))
  println("Pt2: " + pt2(line))
