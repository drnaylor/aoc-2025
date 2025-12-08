package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Fractional.Implicits.infixFractionalOps

object Day04 extends AocDay[Set[(Int, Int)]] {

  override val day: Int = 4

  override type P1 = Int
  override type P2 = Int

  def parseRow(input: String): Set[Int] =
    input.zipWithIndex.filter(x => x._1 == '@').map(_._2).toSet

  override def parse(source: Source): Set[(Int, Int)] = {
    source.getLines().zipWithIndex.flatMap {
      case (input, row) => parseRow(input).map(col => (row, col))
    }.toSet
  }

  private val offsets = (-1 to 1)
    .flatMap(row => (-1 to 1).map(col => (row, col)))
    .filterNot(x => x == (0, 0))

  def selectPaper(parsed: Set[(Int, Int)]): Set[(Int, Int)] = {
    val existing: Map[(Int, Int), Int] = offsets
      .flatMap { case (rowOffset, colOffset) =>
        parsed.map { case (row, col) => (row + rowOffset, col + colOffset) }
      }
      // we only care about positions where paper already exists
      // so we ignore those to reduce grouping
      .filter(parsed.contains)
      .groupBy(identity)
      .map((loc, vals) => (loc, vals.size))

    parsed.filter { inputCoord =>
      // if it doesn't exist in the parse list, it has no neighbours so
      // return zero
      existing.getOrElse(inputCoord, 0) < 4
    }
  }

  // Task is to find spots with more than 4 rolls of paper
  // Algorithm is to take the eight shifts of the set and
  // work out the sums on each roll from there
  override def part1(parsed: Set[(Int, Int)]): Int =
    selectPaper(parsed).size

  override def part2(parsed: Set[(Int, Int)]): Int = {
    @tailrec
    def performStep(parsed: Set[(Int, Int)], removed: Int = 0): Int = {
      val toRemove = selectPaper(parsed)
      if toRemove.nonEmpty then performStep(parsed -- toRemove, removed + toRemove.size) else removed
    }

    performStep(parsed)
  }
}
