package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.io.Source

object Day07 extends AocDay[List[List[Int]]] {

  override val day: Int = 7

  override type P1 = Long
  override type P2 = Long

  override def parse(source: Source): List[List[Int]] =
    source.getLines().map { line =>
      line.zipWithIndex.filter(x => x._1 == '^' || x._1 == 'S').map(_._2).toList
    }.toList


  def splitBeam(row: List[Int])(location: Int): List[Int] =
    row.find(_ == location)
      .map(x => List(x - 1, x + 1))
      .getOrElse(List(location))

  override def part1(parsed: List[List[Int]]): Long = {
    val startingLocation = parsed.head.head

    parsed.tail.foldLeft((Set(startingLocation), 0)) { case ((currentBeams, strikes), row) =>
      val result = currentBeams.toList.flatMap(splitBeam(row))

      // strikes double the beam, so we take the difference and add it to the row
      // this is why we turned it into a list, because we don't want two beams to combine until
      // after we've done that count.
      (result.toSet, strikes + (result.size - currentBeams.size))
    }._2
  }

  override def part2(parsed: List[List[Int]]): Long = {
    val startingLocation = parsed.head.head

    parsed.tail.foldLeft((Map[Int, Long]((startingLocation, 1L)))) { (currentBeams, row) =>
      val result = currentBeams.toList.flatMap { case (pos, number) =>
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
