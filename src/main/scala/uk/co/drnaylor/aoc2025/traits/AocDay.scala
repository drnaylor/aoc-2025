package uk.co.drnaylor.aoc2025.traits

import cats.Show

import scala.io.Source
import scala.util.Using

trait AocDay[P] {

  val day: Int

  final def runDay(onlyFirstPart: Boolean = false): Unit = {
    val filename = f"day$day%02d.txt"
    println(f"Running Day $day%02d")
    println("----")

    Using(Source.fromResource(filename))(parse).map {
      parsedFile =>
        println(f"Part 1: ${part1(parsedFile)}")
        if (!onlyFirstPart) {
          println(f"Part 2: ${part2(parsedFile)}")
        }
    }.recover {
      err =>
        println(s"Error running day: ${err.getMessage}")
        err.printStackTrace()
    }
    println("----")

  }

  type P1: Show
  type P2: Show

  def parse(source: Source): P

  def part1(parsed: P): P1

  def part2(parsed: P): P2

}
