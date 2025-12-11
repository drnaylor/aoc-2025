package uk.co.drnaylor.aoc2025.test

import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day10
import uk.co.drnaylor.aoc2025.Day10.MachineConfiguration

import scala.io.Source

class Day10Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
      |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
      |""".stripMargin


  val parsed1: Seq[MachineConfiguration] =
    Seq(
      MachineConfiguration(
        Set(1, 2),
        Seq(
          Seq(3),
          Seq(1, 3),
          Seq(2),
          Seq(2, 3),
          Seq(0, 2),
          Seq(0, 1)
        ),
        Seq(3, 5, 4, 7)
      ),
      MachineConfiguration(
        Set(3),
        Seq(
          Seq(0, 2, 3, 4),
          Seq(2, 3),
          Seq(0, 4),
          Seq(0, 1, 2),
          Seq(1, 2, 3, 4)
        ),
        Seq(7, 5, 12, 7, 2)
      ),
      MachineConfiguration(
        Set(1, 2, 3, 5),
        Seq(
          Seq(0, 1, 2, 3, 4),
          Seq(0, 3, 4),
          Seq(0, 1, 2, 4, 5),
          Seq(1, 2)
        ),
        Seq(10, 11, 11, 5, 10, 5)
      )
    )

  "Parser" - {

    class WrappedTestFailedException(message: String, cause: TestFailedException) extends TestFailedException(cause.getMessage, cause.failedCodeStackDepth) {
      override def getMessage: String = s"$message \n\ninner message: ${super.getMessage}"
    }

    "can parse the AoC example" in {
      val configurations = Day10.parse(Source.fromString(example))
      try {
        configurations.zip(parsed1).foreach { case (actual, expected) =>
          actual.targetState must contain theSameElementsAs expected.targetState
          actual.buttons must contain theSameElementsAs expected.buttons
          actual.joltage must contain theSameElementsAs expected.joltage
        }
      }
      catch {
        case e: TestFailedException =>
          throw WrappedTestFailedException(s"Failed comparing actual $configurations with expected $parsed1", e)
      }
    }

  }

  "Part 1" - {

    val expectedPresses = Table(
      ("machine", "presses"),
      (parsed1.head, 2),
      (parsed1(1), 3),
      (parsed1.last, 2),
    )

    "can calculate minimum presses" in forAll(expectedPresses) { (input, expected) =>
      Day10.processMachine(input) mustBe expected
    }

    "can calculate AoC example" in {
      Day10.part1(parsed1) mustBe 7
    }

  }

  "Part 2" - {

    val expectedPresses = Table(
      ("machine", "presses"),
      (parsed1.head, 10),
      (parsed1(1), 12),
      (parsed1.last, 11),
    )

    "can calculate minimum presses with OR-Tools" in forAll(expectedPresses) { (input, expected) =>
      Day10.processJoltageOR(input) mustBe expected
    }

    "can calculate minimum presses with Z3" in forAll(expectedPresses) { (input, expected) =>
      Day10.processJoltageZ3(input) mustBe expected
    }

    "can calculate AoC example using Z3" in {
      Day10.part2Z3(parsed1) mustBe 33
    }

    "can calculate AoC example using OR-Tools" in {
      Day10.part2OR(parsed1) mustBe 33
    }
    
  }


}
