package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day03

import scala.io.Source

class Day03Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """987654321111111
      |811111111111119
      |234234234234278
      |818181911112111
      |""".stripMargin

  val parsed: Seq[Seq[Int]] = Seq(
    Seq(9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1),
    Seq(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9),
    Seq(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8),
    Seq(8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1)
  )

  "Parser" - {

    "parses an input correctly" in {
      Day03.parse(Source.fromString(example)) mustBe parsed
    }

  }

  "Part 1" - {

    val joltages = Table(
      ("row", "joltage"),
      parsed.zip(
        Seq(98, 89, 78, 92)
      )*
    )

    "returns the appropriate joltage for a row" in forAll(joltages) { (row, joltage) =>
      Day03.processRow(row) mustBe joltage
    }

    "returns the value expected by the AoC example" in {
      Day03.part1(parsed) mustBe 357
    }

  }

  "Part 2" - {

    val joltages = Table(
      ("row", "joltage"),
      parsed.zip(
        Seq(987654321111L, 811111111119L, 434234234278L, 888911112111L)
      ) *
    )

    "returns the appropriate joltage for a row" in forAll(joltages) { (row, joltage) =>
      Day03.processRowFor12(row) mustBe joltage
    }

    "returns the value expected by the AoC example" in {
      Day03.part2(parsed) mustBe 3121910778619L
    }

  }


}
