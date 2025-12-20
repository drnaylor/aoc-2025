package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day12
import uk.co.drnaylor.aoc2025.Day12.{Bin, Parsed, Present}

import scala.io.Source

class Day12Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """0:
      |###
      |##.
      |##.
      |
      |1:
      |###
      |##.
      |.##
      |
      |2:
      |.##
      |###
      |##.
      |
      |3:
      |##.
      |###
      |##.
      |
      |4:
      |###
      |#..
      |###
      |
      |5:
      |###
      |.#.
      |###
      |
      |4x4: 0 0 0 0 2 0
      |12x5: 1 0 1 0 2 2
      |12x5: 1 0 1 0 3 2
      |
      |""".stripMargin

  val parsed1: Day12.Parsed = Parsed(
    IndexedSeq(
      Present(Set((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (2, 0), (2, 1))),
      Present(Set((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (2, 1), (2, 2))),
      Present(Set((0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1))),
      Present(Set((0, 0), (0, 1), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1))),
      Present(Set((0, 0), (0, 1), (0, 2), (1, 0), (2, 0), (2, 1), (2, 2))),
      Present(Set((0, 0), (0, 1), (0, 2), (1, 1), (2, 0), (2, 1), (2, 2)))
    ),
    Seq(
      Bin(4, 4, Map.from(Seq((4, 2)))),
      Bin(12, 5, Map.from(Seq((0, 1), (2, 1), (4, 2), (5, 2)))),
      Bin(12, 5, Map.from(Seq((0, 1), (2, 1), (4, 3), (5, 2)))),
    )
  )

  "Parser" - {

    "can parse the part 1 AoC example" in {
      Day12.parse(Source.fromString(example)) mustBe parsed1
    }

  }

  "Part 1" - {

    "can calculate AoC example" in {
      // Day12.part1(parsed1) mustBe 2
    }

  }

  "Part 2" - {

    "can calculate AoC example" in {
      // Day12.part2(parsed1) mustBe 2
    }

  }

}
