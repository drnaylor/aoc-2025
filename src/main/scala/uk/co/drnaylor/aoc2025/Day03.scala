package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.io.Source

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

  override type P2 = Long
  override def part2(parsed: List[List[Int]]): Long = ???
}
