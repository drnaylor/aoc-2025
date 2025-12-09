package uk.co.drnaylor.aoc2025.test

import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day01

import scala.io.Source

class Day01Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82
      |""".stripMargin

  val exampleParsed: Seq[Int] = Seq(
    -68,
    -30,
    48,
    -5,
    60,
    -55,
    -1,
    -99,
    14,
    -82
  )

  "Parser" - {
    "should return L values as negative numbers" in forAll(Gen.chooseNum[Int](0, 10000)) { value =>
      val inputString = s"L$value"
      Day01.parseValue(inputString) mustBe Some(-value)
    }

    "should return R values as positive numbers" in forAll(Gen.chooseNum[Int](0, 10000)) { value =>
      val inputString = s"R$value"
      Day01.parseValue(inputString) mustBe Some(value)
    }

    "should return nothing for an empty line" in {
      Day01.parseValue("") mustBe None
    }

    "should parse the example correctly" in {
      Day01.parse(Source.fromString(example)) mustBe exampleParsed
    }
  }

  "Part 1" - {
    "parsing example starting at 50 should give 3" in {
      Day01.part1(exampleParsed) mustBe 3
    }
  }

  "Part 2" - {
    case class TestCase(start: Int, instruction: Int, end: Int, countOfZeroes: Int)

    val valueTable = Table(
      ("start", "instruction", "end", "countOfZero"),
      (50, -68, 82, 1),
      (82, -30, 52, 0),
      (52, 48, 0, 1),
      (0, -5, 95, 0),
      (95, 60, 55, 1),
      (55, -55, 0, 1),
      (0, -1, 99, 0),
      (99, -99, 0, 1),
      (0, 14, 14, 0),
      (14, -82, 32, 1),
      (14, -382, 32, 4)
    )

    "test moving the dial gives expected results" in forAll(valueTable) { case (start, instruction, end, countOfZeroes) =>
      Day01.State(start, 0).turnDial(instruction) mustBe Day01.State(end, countOfZeroes)
    }


    "parsing example starting at 50 should give 6" in {
      Day01.part2(exampleParsed) mustBe 6
    }
  }


}
