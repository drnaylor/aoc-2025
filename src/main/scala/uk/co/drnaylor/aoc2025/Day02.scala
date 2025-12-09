package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.collection.immutable.NumericRange
import scala.io.Source

object Day02 extends AocDay[Seq[NumericRange.Inclusive[Long]]] {

  override type P1 = Long
  override type P2 = Long

  override val day: Int = 2

  def parseIdRange(input: String): NumericRange.Inclusive[Long] = {
    val split = input.split("-", 2).map(_.toLong).iterator
    split.next() to split.next()
  }

  override def parse(source: Source): Seq[NumericRange.Inclusive[Long]] = {
    source.getLines().next().split(",").map(parseIdRange).toSeq
  }

  def checkForRepeat(inputString: String)(chunkSize: Int): Boolean =
    inputString.grouped(chunkSize).distinct.size == 1

  // Part 1

  /**
   * Returns whether a value has a pattern repeated twice in it
   *
   * @param value The value to check
   * @return true if so
   */
  def hasSingleRepeat(value: Long): Boolean = {

    val checkString = value.toString
    val count = checkString.length

    if count % 2 == 0 then checkForRepeat(checkString)(count / 2) else false
  }

  override def part1(parsed: Seq[NumericRange.Inclusive[Long]]): Long = {
    parsed.flatMap(_.iterator).filter(hasSingleRepeat).sum
  }

  // Part 2

  def hasAnyRepeat(value: Long): Boolean = {
    val checkString = value.toString
    val count = checkString.length

    (1 to (count / 2)).filter(count % _ == 0).exists(checkForRepeat(checkString))
  }

  override def part2(parsed: Seq[NumericRange.Inclusive[Long]]): Long = {
    parsed.flatMap(_.iterator).filter(hasAnyRepeat).sum
  }
}
