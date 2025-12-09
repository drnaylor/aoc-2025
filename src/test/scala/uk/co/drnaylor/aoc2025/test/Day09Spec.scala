package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day09
import uk.co.drnaylor.aoc2025.Day09.Coord2d

import scala.io.Source

class Day09Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """7,1
      |11,1
      |11,7
      |9,7
      |9,5
      |2,5
      |2,3
      |7,3
      |""".stripMargin


  val parsed1: Seq[Coord2d] =
    Seq(
      Coord2d(7, 1),
      Coord2d(11, 1),
      Coord2d(11, 7),
      Coord2d(9, 7),
      Coord2d(9, 5),
      Coord2d(2, 5),
      Coord2d(2, 3),
      Coord2d(7, 3)
    )

  "Parser" - {

    "can parse the AoC example" in {
      Day09.parse(Source.fromString(example)) mustBe parsed1
    }

  }

  "Coord2d" - {

    val coords = Table(
      ("left", "right", "area"),
      (Coord2d(2, 5), Coord2d(9, 7), 24),
      (Coord2d(7, 1), Coord2d(11, 7), 35),
      (Coord2d(7, 3), Coord2d(2, 3), 6),
      (Coord2d(2, 5), Coord2d(11, 1), 50)
    )

    "can get the right area" in forAll(coords) { (left, right, area) =>
      left.areaWith(right) mustBe area
    }

  }

  "Part 1" - {

    "can calculate AoC example" in {
      Day09.part1(parsed1) mustBe 50
    }

  }

  "Part 2" - {

    "can calculate AoC example" in {
      Day09.part2(parsed1) mustBe 24
    }

  }


}
