package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.Day09.Coord2d
import uk.co.drnaylor.aoc2025.Day09.Lines.{XLine, YLine}
import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.io.Source
import scala.math.{abs, max, min}

object Day09 extends AocDay[Seq[Coord2d]] {

  case class Coord2d(x: Long, y: Long) {
    def areaWith(that: Coord2d): Long = (abs(this.x - that.x) + 1) * (abs(this.y - that.y) + 1)
  }

  override val day: Int = 9
  override type P1 = Long
  override type P2 = Long

  override def parse(source: Source): Seq[Coord2d] =
    source.getLines().filterNot(_.isEmpty).map { line =>
      line.split(",", 2).map(_.toLong).toList match {
        case x :: y :: Nil => Coord2d(x, y)
        case r => throw IllegalStateException("$r - Should only have two digits")
      }
    }.toSeq

  override def part1(parsed: Seq[Coord2d]): Long = {
    (for {
      (first, idx) <- parsed.zipWithIndex
      second       <- parsed.drop(idx + 1)
    } yield first.areaWith(second)).max
  }

  // Part 2

  object Lines {
    sealed trait Line {
      def intersects(that: Line): Boolean
    }

    case class XLine(x: Long, ymin: Long, ymax: Long) extends Line{
      override def intersects(that: Line): Boolean = that match {
          case that: YLine => intersects0(that)
          case _ => false
        }

      def intersects0(that: YLine): Boolean =
         this.x >= that.xmin && this.x <= that.xmax && that.y >= this.ymin && that.y <= this.ymax
    }

    case class YLine(y: Long, xmin: Long, xmax: Long) extends Line {
      override def intersects(that: Line): Boolean = that match {
        case that: XLine => that.intersects0(this)
        case _ => false
      }
    }
  }

  def determineLines(parsed: Seq[Coord2d]): Set[Lines.Line] = {
    parsed.appended(parsed.head).sliding(2, 1).flatMap {
      case Seq(a, b) if a.x == b.x => Some(XLine(a.x, min(a.y, b.y), max(a.y, b.y)))
      case Seq(a, b) if a.y == b.y => Some(YLine(a.y, min(a.x, b.x), max(a.x, b.x)))
      case _ => None
    }.toSet
  }

  // As the shape is a polygon, we basically check to see if any edges of the polygons
  // intersect the rectangle. We only need to do the perimeter of the rectangle.
  def isValidRectangle(lines: Set[Lines.Line])(first: Coord2d, second: Coord2d): Boolean = {
    // We want true ONLY if no intersects are found, so this all needs to be negated
    // as this will return if there IS an intersection
    !(Seq(
      XLine(first.x, min(first.y, second.y) + 1, max(first.y, second.y) - 1),
      XLine(second.x, min(first.y, second.y) + 1, max(first.y, second.y) - 1),
      YLine(first.y, min(first.x, second.x) + 1, max(first.x, second.x) - 1),
      YLine(second.y, min(first.x, second.x) + 1, max(first.x, second.x) - 1),
    ).exists {
      l => lines.exists(l.intersects)
    })
  }

  override def part2(parsed: Seq[Coord2d]): Long = {
    val checkIsValid = isValidRectangle(determineLines(parsed))

    (for {
      (first, idx) <- parsed.zipWithIndex
      second       <- parsed.drop(idx + 1)
      area         =  first.areaWith(second)
    } yield (first, second, area)).sortBy(-_._3).find {
      case (first, second, _) => checkIsValid(first, second)
    }.map {
      case (_, _, area) => area
    }.get
  }
}
