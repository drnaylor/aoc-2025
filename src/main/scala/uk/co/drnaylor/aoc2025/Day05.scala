package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.annotation.{showAsInfix, tailrec}
import scala.math.{max, min}
import scala.io.Source

// Doing this because NumericRange.Inclusive can't handle ranges that are larger than Int.MaxValue
// for some reason...
object IDRange {
  def apply(start: Long, end: Long): IDRange = {
    if (start > end) throw IllegalArgumentException("start must not be greater than end")
    else new IDRange(start, end)
  }
}
case class IDRange private (start: Long, end: Long) {
  def contains(value: Long): Boolean = value >= start && value <= end

  infix def canMergeWith(that: IDRange): Boolean = {
    this.contains(that.start) || this.contains(that.end) || that.contains(this.start) || that.contains(this.end)
  }

  def mergeIfOverlap(that: IDRange): Option[IDRange] = {
    if (this canMergeWith that) {
      Some(IDRange(min(this.start, that.start), max(this.end, that.end)))
    } else None
  }

  lazy val size: Long = (end - start) + 1L

  override lazy val toString: String = s"IDRange($start-$end)"
}

case class Day05Parsed(ranges: Set[IDRange], available: Set[Long])

object Day05 extends AocDay[Day05Parsed] {

  override val day: Int = 5

  override type P1 = Int
  override type P2 = Long

  // From day 02
  def parseIdRange(input: String): IDRange = {
    val split = input.split("-", 2).map(_.toLong).iterator
    IDRange(split.next(), split.next())
  }

  override def parse(source: Source): Day05Parsed =
    source.getLines().foldLeft(Day05Parsed(Set(), Set())) {
        case (current, "") => current
        case (current, line) if line.contains("-") => current.copy(ranges = current.ranges + parseIdRange(line))
        case (current, line) => current.copy(available = current.available + line.toLong)
    }

  override def part1(parsed: Day05Parsed): Int =
    parsed.available.count { id =>
      parsed.ranges.exists(_.contains(id))
    }

  def sortAndMerge(initial: Set[IDRange]): List[IDRange] = {
    // We have a handful of values and we can reduce passes down the list to one
    // if we sort first.
    val values = initial.toList.sortBy(_.start)

    @tailrec
    def mergeIfAppropriate(first: IDRange, second: IDRange, cons: List[IDRange], drain: List[IDRange]): List[IDRange] = {
      first.mergeIfOverlap(second) match {
        // If we get a merge, this becomes first, and second is taken from the head of cons if it exists
        case Some(result) if cons.nonEmpty => mergeIfAppropriate(result, cons.head, cons.tail, drain)
        // If we don't get a merge, and cons is not empty, first goes onto drain, second becomes first, and the head of cons is second
        case None if cons.nonEmpty => mergeIfAppropriate(second, cons.head, cons.tail, drain.appended(first))
        // If cons is empty and we have a result, then we attach this to the end of drain and return that
        // Otherwise, attach first and second to the drain and return it
        case Some(result) => drain.appended(result)
        case None => drain ++ List(first, second)
      }
    }

    values match {
      case first :: second :: tail => mergeIfAppropriate(first, second, tail, List.empty[IDRange])
      case _ => throw IllegalStateException("This should not happen")
    }
  }

  override def part2(parsed: Day05Parsed): Long = {
    sortAndMerge(parsed.ranges).map(_.size).sum
  }
}
