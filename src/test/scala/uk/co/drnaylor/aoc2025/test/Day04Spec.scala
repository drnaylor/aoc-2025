package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day04

import scala.io.Source

class Day04Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """..@@.@@@@.
      |@@@.@.@.@@
      |@@@@@.@.@@
      |@.@@@@..@.
      |@@.@@@@.@@
      |.@@@@@@@.@
      |.@.@.@.@@@
      |@.@@@.@@@@
      |.@@@@@@@@.
      |@.@.@@@.@.
      |""".stripMargin

  // row then column
  val parsed: Set[(Int, Int)] =
    Set(
      (0, 2), (0, 3), (0, 5), (0, 6), (0, 7), (0, 8),
      (1, 0), (1, 1), (1, 2), (1, 4), (1, 6), (1, 8), (1, 9),
      (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 6), (2, 8), (2, 9),
      (3, 0), (3, 2), (3, 3), (3, 4), (3, 5), (3, 8),
      (4, 0), (4, 1), (4, 3), (4, 4), (4, 5), (4, 6), (4, 8), (4, 9),
      (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (5, 7), (5, 9),
      (6, 1), (6, 3), (6, 5), (6, 7), (6, 8), (6, 9),
      (7, 0), (7, 2), (7, 3), (7, 4), (7, 6), (7, 7), (7, 8), (7, 9),
      (8, 1), (8, 2), (8, 3), (8, 4), (8, 5), (8, 6), (8, 7), (8, 8),
      (9, 0), (9, 2), (9, 4), (9, 5), (9, 6), (9, 8)
    )

  "Parser" - {

    val testData = Table(
      ("input", "output"),
      ("......", Set()),
      ("..@..", Set(2)),
      (".@..@...", Set(1, 4)),
      ("..@....@...", Set(2, 7))
    )

    "can parse a row" in forAll(testData) { (input, output) =>
      Day04.parseRow(input) mustBe output
    }

    "can parse the AoC example" in {
      Day04.parse(Source.fromString(example)) mustBe parsed
    }

  }

  "Part 1" - {

    // Added because my algorithm orignally was too low, and I realised
    // that using "groupBy" returned only locations that had at least one
    // neighbour
    "can handle paper with no neighbours" in {
      Day04.part1(Set((1, 1))) mustBe 1
    }

    "can calculate AoC example" in {
      Day04.part1(parsed) mustBe 13
    }

  }

  "Part 2" - {

    "can calculate AoC example" in {
      Day04.part2(parsed) mustBe 43
    }

  }


}
