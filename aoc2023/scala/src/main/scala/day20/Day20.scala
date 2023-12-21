package day20

import day20.Signal.Low
import day20.State.{Off, On}

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

enum State:
  case On, Off

enum Signal:
  case High, Low

sealed trait Module:
  def label: String

  def destinations: List[String]

  def allDestinations: List[String]

  def signal: Signal

  def receive(signal: Signal, label: String): (Module, List[String])

case class FlipFlop(label: String, destinationList: List[String], currentState: State = Off) extends Module:
  override def allDestinations: List[String] = destinationList

  override def destinations: List[String] = currentState match
    case Off => List()
    case On => destinationList

  override def signal: Signal = currentState match
    case Off => Signal.Low
    case On => Signal.High

  override def receive(signal: Signal, label: String): (FlipFlop, List[String]) = signal match
    case Signal.High => (this, List())
    case Signal.Low => (FlipFlop(label, destinationList, flip(currentState)), destinationList)

case class Conjunction(label: String, destinations: List[String], connections: Map[String, Signal]) extends Module:
  override def allDestinations: List[String] = destinations

  override def signal: Signal =
    if connections.nonEmpty && connections.values.forall(_ == Signal.High) then Signal.Low
    else Signal.High

  override def receive(signal: Signal, label: String): (Conjunction, List[String]) =
    (Conjunction(label, destinations, connections + (label -> signal)), destinations)

case class Broadcast(label: String, destinations: List[String], lastReceived: Signal = Signal.Low) extends Module:
  override def allDestinations: List[String] = destinations

  override def signal: Signal = lastReceived

  override def receive(signal: Signal, label: String): (Broadcast, List[String]) = (Broadcast(label, destinations, signal), destinations)

type Modules = Map[String, Module]

def flip(state: State): State = state match
  case Off => On
  case On => Off

def pt1(lines: List[String]): Int =
  val modules = updateConjunctions(parseInput(lines))
  val (_, (low, high)) = (1 to 1000).foldLeft((modules, (0, 0)))((acc, _) => processSignals(List(("", Signal.Low, "broadcaster")), acc._1, acc._2))
  low * high

def pt2(lines: List[String]): BigInt =
  var modules = updateConjunctions(parseInput(lines))
  var isDone = false
  var count = BigInt(1)
  var th: Option[BigInt] = None
  var sv: Option[BigInt] = None
  var gh: Option[BigInt] = None
  var ch: Option[BigInt] = None
  while (!isDone) {
    val (updatedModules, signals) = processSignals2(List(("", Signal.Low, "broadcaster")), modules)
    if signals.contains("th") && th.isEmpty then th = Some(count)
    if signals.contains("sv") && sv.isEmpty then sv = Some(count)
    if signals.contains("gh") && gh.isEmpty then gh = Some(count)
    if signals.contains("ch") && ch.isEmpty then ch = Some(count)
    isDone = th.isDefined && sv.isDefined && gh.isDefined && ch.isDefined
    modules = updatedModules
    count = count + 1
  }
  th.get * sv.get * gh.get * ch.get

def updateConjunctions(modules: Modules): Modules =
  val conjunctions = modules.filter(_._2.isInstanceOf[Conjunction])
  val sendReceivePair = modules.flatMap((label, module) => module.allDestinations.map((label, _)))
  val sendReceiveToConjunction = sendReceivePair.filter((_, to) => conjunctions.contains(to))
  val toConjunction = sendReceiveToConjunction.groupBy(_._2).map((label, pairs) => label -> pairs.keys.toList)
  toConjunction.foldLeft(modules)((acc, value) => updateConjunction(acc, value))

def updateConjunction(modules: Modules, value: (String, List[String])): Modules =
  val (label, newConnections) = value
  val conjunction = modules(label)
  val value1 = newConnections.map(_ -> Signal.Low)
  val updatedConjunction = Conjunction(conjunction.label, conjunction.allDestinations, connections = value1.toMap)
  modules + (label -> updatedConjunction)

def updateSignalCount(signalCount: (Int, Int), signal: Signal) = {
  val updatedSignalCount = signal match
    case Signal.High => (signalCount._1 + 1, signalCount._2)
    case Signal.Low => (signalCount._1, signalCount._2 + 1)
  updatedSignalCount
}
@tailrec
def processSignals(signals: List[(String, Signal, String)], modules: Modules, signalCount: (Int, Int)): (Modules, (Int, Int)) =
  signals.headOption match
    case None => (modules, signalCount)
    case Some((origin, signal, label)) if modules.contains(label) =>
      val module = modules(label)
      val (updatedModule, destinations) = module.receive(signal, origin)
      val updatedSignalCount: (Int, Int) = updateSignalCount(signalCount, signal)
      val newSignals = destinations.map((label, updatedModule.signal, _))
      processSignals(signals.tail ++ newSignals, modules + (label -> updatedModule), updatedSignalCount)
    case Some((_, signal, _)) =>
      val updatedSignalCount: (Int, Int) = updateSignalCount(signalCount, signal)
      processSignals(signals.tail, modules, updatedSignalCount)

@tailrec
def processSignals2(signals: List[(String, Signal, String)], modules: Modules, endSignals: List[String] = List()): (Modules, List[String]) =
  signals.headOption match
    case None => (modules, endSignals)
    case Some((_, Signal.Low, "th")) => processSignals2(signals.tail, modules, endSignals :+ "th")
    case Some((_, Signal.Low, "sv")) => processSignals2(signals.tail, modules, endSignals :+ "sv")
    case Some((_, Signal.Low, "gh")) => processSignals2(signals.tail, modules, endSignals :+ "gh")
    case Some((_, Signal.Low, "ch")) => processSignals2(signals.tail, modules, endSignals :+ "ch")
    case Some((origin, signal, label)) if modules.contains(label) =>
      val module = modules(label)
      val (updatedModule, destinations) = module.receive(signal, origin)
      val newSignals = destinations.map((label, updatedModule.signal, _))
      processSignals2(signals.tail ++ newSignals, modules + (label -> updatedModule), endSignals)
    case Some((_, _, _)) =>
      processSignals2(signals.tail, modules, endSignals)

def parseInput(lines: List[String]): Modules =
  lines.map(parseLine).toMap

def parseLine(line: String): (String, Module) = line match
  case s"%$label -> $destinations" => label -> FlipFlop(label, destinations.split(", ").toList)
  case s"&$label -> $destinationStr" =>
    val destinations = destinationStr.split(", ").toList
    label -> Conjunction(label, destinations, Map())
  case s"$label -> $destinations" =>
    label -> Broadcast(label, destinations.split(", ").toList)
  case _ => throw new Exception(s"Invalid line: $line")

@main
def main(): Unit =
  val lines = Source.fromResource("day20.txt").getLines().map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
