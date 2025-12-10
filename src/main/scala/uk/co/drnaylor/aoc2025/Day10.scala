package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.Day09.Coord2d
import uk.co.drnaylor.aoc2025.Day09.Lines.{XLine, YLine}
import uk.co.drnaylor.aoc2025.Day10.MachineConfiguration
import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.annotation.tailrec
import scala.io.Source
import scala.math.{abs, max, min}
import scala.util.matching.Regex

object Day10 extends AocDay[Seq[MachineConfiguration]] {

  case class MachineConfiguration(targetState: Set[Int], buttons: Seq[Seq[Int]], joltage: Seq[Int])

  override val day: Int = 10
  override type P1 = Int
  override type P2 = Long

  private val parseRegex: Regex = """\[([.#]+)] (.+) \{([0-9,]+)}""".r

  def parseMachine(input: String): Set[Int] =
    input.zipWithIndex.filter(_._1 == '#').map(_._2).toSet

  def parseButtons(input: String): Seq[Seq[Int]] =
    """\(([\d,]+)\)""".r.findAllMatchIn(input).map { m =>
      m.group(1).split(",").map(_.toInt).toSeq
    }.toSeq

  def parseJoltage(input: String): Seq[Int] =
    input.split(",").map(_.toInt)

  override def parse(source: Source): Seq[MachineConfiguration] =
    source.getLines().filterNot(_.isEmpty).map {
      case parseRegex(machine, buttons, joltage) =>
        MachineConfiguration(
          parseMachine(machine),
          parseButtons(buttons),
          parseJoltage(joltage)
        )
    }.toSeq


  private def processMachine1(machineConfiguration: MachineConfiguration): Int = machineConfiguration match {
    case MachineConfiguration(machine, buttons, _) =>
      // we don't need to press any buttons more than once, using a "breadth first" approach
      // order doesn't matter due to the binary nature of the solution, we just need to
      // find the first result that works if we go from one button only upwards.

      @tailrec
      def getCombinations(buttons: Seq[Seq[Int]], requiredSequence: Set[Int], length: Int, current: Int): Int = {
        // we want to flatten each combination's inner entry to be just a list of numbers
        // we then group them and get the numbers that have odd values only, as they've
        // been flipped
        buttons.combinations(current).map(_.flatten).map { buttonSequence =>
          buttonSequence.groupBy(identity).flatMap {
            case (key, value) if value.size % 2 == 1 => Some(key)
            case _ => None
          }.toSet
        }.contains(requiredSequence) match {
          case true => current
          case false if current == length => throw new IllegalStateException("That didn't work...")
          case false => getCombinations(buttons, requiredSequence, length, current + 1)
        }
      }

      getCombinations(buttons, machine, buttons.length, 1)
  }


  override def part1(parsed: Seq[MachineConfiguration]): Int =
    parsed.map(processMachine1).sum

  override def part2(parsed: Seq[MachineConfiguration]): Long = ???
}
