package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.io.Source

object Day07 extends AocDay[Seq[Seq[Int]]] {

  override val day: Int = 7

  override type P1 = Long
  override type P2 = Long

  override def parse(source: Source): Seq[Seq[Int]] =
    source.getLines().map { line =>
      line.zipWithIndex.filter(x => x._1 == '^' || x._1 == 'S').map(_._2).toSeq
    }.toSeq


  def splitBeam(row: Seq[Int])(location: Int): Seq[Int] =
    row.find(_ == location)
      .map(x => Seq(x - 1, x + 1))
      .getOrElse(Seq(location))

  override def part1(parsed: Seq[Seq[Int]]): Long = {
    val startingLocation = parsed.head.head

    parsed.tail.foldLeft((Set(startingLocation), 0)) { case ((currentBeams, strikes), row) =>
      val result = currentBeams.toSeq.flatMap(splitBeam(row))

      // strikes double the beam, so we take the difference and add it to the row
      // this is why we turned it into a list, because we don't want two beams to combine until
      // after we've done that count.
      (result.toSet, strikes + (result.size - currentBeams.size))
    }._2
  }

  override def part2(parsed: Seq[Seq[Int]]): Long = {
    val startingLocation = parsed.head.head

    parsed.tail.foldLeft((Map[Int, Long]((startingLocation, 1L)))) { (currentBeams, row) =>
      val result = currentBeams.toSeq.flatMap { case (pos, number) =>
        // This allows us to track how many beams are in the same place, reducing the
        // number of iterations by a lot!
        // We use a long as this does overflow!
        splitBeam(row)(pos).map(p => (p, number))
      }.foldLeft(Map.empty[Int, Long]) { case (currentMap, (column, number)) =>
        currentMap.updatedWith(column) {
          case Some(v) => Some(v + number)
          case None => Some(number)
        }
      }
      result
    }.values.sum
  }
}
