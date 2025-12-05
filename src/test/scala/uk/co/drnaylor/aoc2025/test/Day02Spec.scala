package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableFor2
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day02

import scala.collection.immutable.NumericRange
import scala.io.Source

class Day02Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
      |""".stripMargin

  val rangeList: List[(NumericRange.Inclusive[Long], List[Long], List[Long])] = List(
    (11L to 22L, List(11, 22), List(11, 22)),
    (95L to 115L, List(99), List(99, 111)),
    (998L to 1012L, List(1010), List(999, 1010)),
    (1188511880L to 1188511890L, List(1188511885), List(1188511885)),
    (222220L to 222224L, List(222222), List(222222)),
    (1698522L to 1698528L, List(), List()),
    (446443L to 446449L, List(446446), List(446446)),
    (38593856L to 38593862L, List(38593859), List(38593859)),
    (565653L to 565659L, List(), List(565656)),
    (824824821L to 824824827L, List(), List(824824824)),
    (2121212118L to 2121212124L, List(), List(2121212121))
  )

  val rangeTable: TableFor2[NumericRange.Inclusive[Long], List[Long]] = Table(
    ("ranges", "invalid ids"),
    rangeList.map{ case (first, second, _) => (first, second) }*
  )

  val rangeTablePart2: TableFor2[NumericRange.Inclusive[Long], List[Long]] = Table(
    ("ranges", "invalid ids"),
    rangeList.map{ case (first, _, second) => (first, second) }*
  )

  "Parser" - {

    "matches the example on the AoC page" in {
      Day02.parse(Source.fromString(example)) mustBe rangeList.map(_._1)
    }

  }

  "Part 1" - {

    val repeatValues = Table(
      ("value", "isRepeat"),
      (11, true),
      (12, false),
      (22, true),
      (101, false),
      (111, false),
      (1010, true),
      (10101, false),
      (101010, false),
      (100100, true),
    )

    "hasSingleRepeat returns a value for repeated patterns" in forAll(repeatValues) { (value, isRepeat) =>
      Day02.hasSingleRepeat(value) mustBe isRepeat
    }

    "each range list contains returns the appropriate list of invalid IDs" in forAll(rangeTable) { case (range, invalids) =>
      range.filter(Day02.hasSingleRepeat) must contain theSameElementsAs invalids
    }

    "matches the example on the AoC page" in {
      Day02.part1(rangeList.map(_._1)) mustBe 1227775554L
    }

  }

  "Part 2" - {

    val repeatValues = Table(
      ("value", "isRepeat"),
      (11, true),
      (12, false),
      (22, true),
      (101, false),
      (111, true),
      (1010, true),
      (10101, false),
      (101010, true),
      (100100, true),
    )

    "hasAnyRepeat returns a value for repeated patterns" in forAll(repeatValues) { (value, isRepeat) =>
      Day02.hasAnyRepeat(value) mustBe isRepeat
    }

    "each range list contains returns the appropriate list of invalid IDs" in forAll(rangeTablePart2) { case (range, invalids) =>
      range.filter(Day02.hasAnyRepeat) must contain theSameElementsAs invalids
    }

    "matches the example on the AoC page" in {
      Day02.part2(rangeList.map(_._1)) mustBe 4174379265L
    }

  }


}
