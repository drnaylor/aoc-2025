package uk.co.drnaylor.aoc2025.traits

import cats.Show

import scala.io.Source

trait AocDay[P] {

  val day: Int

  final def runDay(): Unit = {
    val filename = f"day$day%02d.txt"
    println(f"Running Day $day%02d")
    println("----")
    val parsedFile = parse(Source.fromResource(filename))
    println(f"Part 1: ${part1(parsedFile)}")
    println(f"Part 2: ${part2(parsedFile)}")
  }

  type P1: Show
  type P2: Show

  def parse(source: Source): P

  def part1(parsed: P): P1

  def part2(parsed: P): P2

}
