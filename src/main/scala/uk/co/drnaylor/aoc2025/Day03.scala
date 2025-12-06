package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Fractional.Implicits.infixFractionalOps

object Day03 extends AocDay[List[List[Int]]] {

  override val day: Int = 3

  override def parse(source: Source): List[List[Int]] =
    source.getLines().filter(_.nonEmpty).map { line =>
        line.map(_.asDigit).toList
    }.toList

  override type P1 = Long
  def processRow(row: List[Int]): Long = {
    // Group by the values, then sort them by their groups
    val groupsByValue: List[(Int, List[Int])] = row.zipWithIndex.groupMap(_._1)(_._2).toList.sortBy(x => -x._1)

    val first = groupsByValue.head

    if (first._2.length > 1) {
      // that's our answer, double biggest number
      first._1 * 10 + first._1
    } else if (first._2.head == row.length - 1) {
      // The largest number is only at the end, so that's our second digit. The first digit is just
      // the next group down.
      groupsByValue(1)._1 * 10 + first._1
    } else {
      // The largest number goes first. We then find the next number that has an index AFTER the first
      // As the list is ordered, we should just get the largest number
      val findAfter = first._2.head

      // This returns an Option, but we know there logically MUST be a value so we unwrap it
      val secondNumber = groupsByValue.find {
        case (_, indexList) => indexList.exists(findAfter < _)
      }.get

      first._1 * 10 + secondNumber._1
    }
  }

  override def part1(parsed: List[List[Int]]): Long = {
    parsed.map(processRow).sum
  }

  // Part 2
  override type P2 = Long

  def processRowFor12(row: List[Int]): Long = {
    // Group by the values, then sort them by their groups
    // val groupsByValue: List[(Int, List[Int])] = row.zipWithIndex.groupMap(_._1)(_._2).toList.sortBy(x => -x._1)
    row.drop(12).foldLeft(row.take(12)) { (currentValue: List[Int], next: Int) =>
      // iterate through current value, for the first difference that is negative, drop it
      // if we don't get anything, take the highest end value
      currentValue.sliding(2, 1).zipWithIndex.flatMap {
        case (list, index) =>
          if (list(1) - list.head > 0) then Some(index) else None
      }.nextOption() match {
        case Some(idx) => {
          currentValue.take(idx) ++ currentValue.slice(idx + 1, 13) ++ List(next)
        }
        case None if currentValue.last < next =>  currentValue.take(11) ++ List(next)
        case None => currentValue
      }
    }.foldLeft(0L) { (currentValue, next) =>
      currentValue * 10L + next
    }
  }

  override def part2(parsed: List[List[Int]]): Long =
    parsed.map(processRowFor12).sum
}
