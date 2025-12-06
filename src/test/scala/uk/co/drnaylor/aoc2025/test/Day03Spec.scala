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

  val parsed: List[List[Int]] = List(
    List(9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1),
    List(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9),
    List(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8),
    List(8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1)
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
        List(98, 89, 78, 92)
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

  }


}
