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


  val parsed1: List[List[Int]] =
    List(
      List(7),
      List.empty,
      List(7),
      List.empty,
      List(6, 8),
      List.empty,
      List(5, 7, 9),
      List.empty,
      List(4, 6, 10),
      List.empty,
      List(3, 5, 9, 11),
      List.empty,
      List(2, 6, 12),
      List.empty,
      List(1, 3, 5, 7, 9, 13),
      List.empty
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
