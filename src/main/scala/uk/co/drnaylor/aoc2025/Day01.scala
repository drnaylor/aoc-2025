package uk.co.drnaylor.aoc2025

import cats.Show
import cats.implicits.given
import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.io.Source
import scala.math.abs

object Day01 extends AocDay[List[Int]] {

  override val day: Int = 1

  override def parse(source: Source): List[Int] = {
      source.getLines().flatMap(parseValue).toList
    }

  def parseValue(input: String): Option[Int] =
    input match {
      case s"L$value" => Some(-value.toInt)
      case s"R$value" => Some(value.toInt)
      case _ => None
    }

  override type P1 = Int

  override def part1(parsed: List[Int]): Int = {
    parsed.scanLeft(50) {
      (acc, curr) => acc + curr
    }.count(_ % 100 == 0)
  }

  // Part 2
  override type P2 = Int

  case class State(currentDialPosition: Int, countPastZero: Int) {
    def turnDial(turnRightBy: Int): State = {
      // we know we'll pass 0 for each revolution, so we just take that
      // out of the equation now and consider the part revolution
      val completeRevolutions = abs(turnRightBy) / 100
      val partRevolution = turnRightBy % 100
      val isLeftTurn = turnRightBy < 0

      // mirror so we just need to consider right turn semantics
      val normalisedValue: Int = if (isLeftTurn && currentDialPosition != 0) {
        100 - currentDialPosition
      } else currentDialPosition

      val afterTurn = normalisedValue + abs(partRevolution)
      val additionalRevolutions = afterTurn / 100

      // Our new position in the right hand world is the after turn value mod 100,
      // but we also need to ensure we return to the left hand world if we switched it around
      val newPosition = (afterTurn % 100) match {
        case 0 => 0
        case a if isLeftTurn => 100 - a
        case a => a
      }

      State(newPosition, countPastZero + completeRevolutions + additionalRevolutions)
    }
  }

  override def part2(parsed: List[Int]): Int =
    parsed.foldLeft(State(50, 0)) {
      (state: State, next: Int) => state.turnDial(next)
    }.countPastZero
}
