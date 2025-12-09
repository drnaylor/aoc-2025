package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day06
import uk.co.drnaylor.aoc2025.Day06.Column

import scala.io.Source

class Day06Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """123 328  51 64
      | 45 64  387 23
      |  6 98  215 314
      |*   +   *   +
      |""".stripMargin


  val parsed1: List[Column] =
    List(
      Column(List(123L, 45L, 6L), Day06.Operations.Multiply),
      Column(List(328L, 64L, 98L), Day06.Operations.Add),
      Column(List(51L, 387L, 215L), Day06.Operations.Multiply),
      Column(List(64L, 23L, 314L), Day06.Operations.Add)
    )

  val parsed2: List[Column] =
    List(
      Column(List(1L, 24L, 356L), Day06.Operations.Multiply),
      Column(List(369L, 248L, 8L), Day06.Operations.Add),
      Column(List(32L, 581L, 175L), Day06.Operations.Multiply),
      Column(List(623L, 431L, 4L), Day06.Operations.Add)
    )

  "Part 1" - {

    "can parse the AoC example" in {
      Day06.parseOne(Day06.parse(Source.fromString(example))) mustBe parsed1
    }

    "can calculate AoC example" in {
      Day06.completeOperation(parsed1) mustBe 4277556
    }

  }

  "Part 2" - {
    "can parse the AoC example" in {
      Day06.parseTwo(Day06.parse(Source.fromString(example))) mustBe parsed2
    }

    "can calculate AoC example" in {
      Day06.completeOperation(parsed2) mustBe 3263827
    }
  }


}
