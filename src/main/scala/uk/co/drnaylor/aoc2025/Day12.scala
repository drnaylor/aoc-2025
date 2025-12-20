package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.Day12.Parsed
import uk.co.drnaylor.aoc2025.Day12.Tristate.{False, True, Unknown}
import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.io.Source
import scala.util.matching.Regex

object Day12 extends AocDay[Parsed] {

  case class Present(shape: Set[(Int, Int)]) extends AnyVal
  case class Bin(width: Int, height: Int, requiredPresents: Map[Int, Int])

  case class Parsed(presents: IndexedSeq[Present], bins: Seq[Bin])

  override val day: Int = 12
  override type P1 = Long
  override type P2 = Long

  // Parsing logic
  def presentRow(row: String): Set[Int] =
    row.zipWithIndex.filter(_._1 == '#').map(_._2).toSet

  private val indexLine: Regex = """\d:""".r
  private val binsLine: Regex = """(\d+)x(\d+): ([\d\s]+)""".r

  override def parse(source: Source): Parsed = {
    val (strings, transient) = source.getLines().foldLeft((Seq.empty[Seq[String]], Seq.empty[String])) { case ((splitLines, transient), line) =>
      line match {
        case "" => (splitLines.appended(transient), Seq.empty[String])
        case indexLine() => (splitLines, transient)
        case text => (splitLines, transient.appended(text))
      }
    }

    (if transient.isEmpty then strings else strings.appended(transient)).toList match {
      case presents :+ bins =>
        Parsed(
          presents.map { present =>
            present.zipWithIndex.flatMap { (row, idx) =>
              presentRow(row).map(col => (idx, col))
            }.toSet
          }.map(Present.apply).toIndexedSeq,
          bins.map { case binsLine(width, height, values) =>
            Bin(width.toInt, height.toInt, values.trim.split(" ").zipWithIndex.flatMap {
              case ("0", _) => None
              case (value, idx) => Some((idx, value.toInt))
            }.toMap)
          }
        )
      case _ => throw IllegalStateException("Nope")
    }
  }

  enum Tristate {
    case True
    case False
    case Unknown
  }

  def checkGrid(presents: IndexedSeq[Present])(binToCheck: Bin): Tristate = {
    val presentMap: Map[Present, Int] = binToCheck.requiredPresents.map((presentId, count) => (presents(presentId), count))
    val area = binToCheck.width * binToCheck.height

    // Simple case, if we treat all presents as 3x3 squares, would they all fit?
    if (9 * presentMap.values.sum) <= area then True
    // If the sum of the areas of the presents is bigger than the area available?
    else if presentMap.map((present, count) => present.shape.size * count).sum > area then False
    else Unknown // TODO: check packing if we need it
  }

  override def part1(parsed: Parsed): Long = {
    val checkFn = checkGrid(parsed.presents)
    parsed.bins.map(checkFn).count(_ == True)
  }

  override def part2(parsed: Parsed): Long = ???
}
