package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day07

import scala.io.Source

class Day07Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """.......S.......
      |...............
      |.......^.......
      |...............
      |......^.^......
      |...............
      |.....^.^.^.....
      |...............
      |....^.^...^....
      |...............
      |...^.^...^.^...
      |...............
      |..^...^.....^..
      |...............
      |.^.^.^.^.^...^.
      |...............
      |""".stripMargin


  val parsed1: Seq[Seq[Int]] =
    Seq(
      Seq(7),
      Seq.empty,
      Seq(7),
      Seq.empty,
      Seq(6, 8),
      Seq.empty,
      Seq(5, 7, 9),
      Seq.empty,
      Seq(4, 6, 10),
      Seq.empty,
      Seq(3, 5, 9, 11),
      Seq.empty,
      Seq(2, 6, 12),
      Seq.empty,
      Seq(1, 3, 5, 7, 9, 13),
      Seq.empty
    )

  "Parser" - {

    "can parse the AoC example" in {
      Day07.parse(Source.fromString(example)) mustBe parsed1
    }

  }


  "Part 1" - {

    "can calculate AoC example" in {
      Day07.part1(parsed1) mustBe 21
    }

  }

  "Part 2" - {

    "can calculate AoC example" in {
      Day07.part2(parsed1) mustBe 40
    }

  }


}
