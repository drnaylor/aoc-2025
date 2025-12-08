package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.{Day05, Day05Parsed, IDRange}

import scala.io.Source

class Day05Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """3-5
      |10-14
      |16-20
      |12-18
      |
      |1
      |5
      |8
      |11
      |17
      |32
      |""".stripMargin

  val parsed: Day05Parsed =
    Day05Parsed(
      ranges = Set(
        IDRange(3L, 5L),
        IDRange(10L, 14L),
        IDRange(16L, 20L),
        IDRange(12L, 18L)
      ),
      available = Set(
        1L,
        5L,
        8L,
        11L,
        17L,
        32L
      )
    )


  "Parser" - {

    "can parse the AoC example" in {
      Day05.parse(Source.fromString(example)) mustBe parsed
    }

  }

  "Part 1" - {

    "can calculate AoC example" in {
      Day05.part1(parsed) mustBe 3
    }

  }

  "Part 2" - {

    "IDRange" - {
      "merges two entries with the same start" in {
        IDRange(12962748711021L, 17106469220194L).mergeIfOverlap(IDRange(12962748711021L, 13823901556693L)) mustBe Some(IDRange(12962748711021L, 17106469220194L))
      }

      "merges two entries with the same end" in {
        IDRange(104123582918543L, 104968750841600L).mergeIfOverlap(IDRange(104720320730834L, 104968750841600L)) mustBe Some(IDRange(104123582918543L, 104968750841600L))
      }

      "merges two entries that overlap" in {
        IDRange(3L, 5L).mergeIfOverlap(IDRange(4L, 7L)) mustBe Some(IDRange(3L, 7L))
      }

      "merges two entries where the first is a subset" in {
        IDRange(3L, 5L).mergeIfOverlap(IDRange(2L, 7L)) mustBe Some(IDRange(2L, 7L))
      }

      "merges two entries where the first is a superset" in {
        IDRange(1L, 9L).mergeIfOverlap(IDRange(2L, 7L)) mustBe Some(IDRange(1L, 9L))
      }

      "does not merge two entries that are not overlapping" in {
        IDRange(3L, 5L).mergeIfOverlap(IDRange(6L, 7L)) mustBe None
      }
    }

    "can calculate AoC example" in {
      Day05.part2(parsed) mustBe 14
    }

  }


}
