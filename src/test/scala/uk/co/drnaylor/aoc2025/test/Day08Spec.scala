package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day08
import uk.co.drnaylor.aoc2025.Day08.Point

import scala.io.Source

class Day08Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """162,817,812
      |57,618,57
      |906,360,560
      |592,479,940
      |352,342,300
      |466,668,158
      |542,29,236
      |431,825,988
      |739,650,466
      |52,470,668
      |216,146,977
      |819,987,18
      |117,168,530
      |805,96,715
      |346,949,466
      |970,615,88
      |941,993,340
      |862,61,35
      |984,92,344
      |425,690,689
      |""".stripMargin


  val parsed1: List[Point] =
    List(
      Point(162,817,812),
      Point(57,618,57),
      Point(906,360,560),
      Point(592,479,940),
      Point(352,342,300),
      Point(466,668,158),
      Point(542,29,236),
      Point(431,825,988),
      Point(739,650,466),
      Point(52,470,668),
      Point(216,146,977),
      Point(819,987,18),
      Point(117,168,530),
      Point(805,96,715),
      Point(346,949,466),
      Point(970,615,88),
      Point(941,993,340),
      Point(862,61,35),
      Point(984,92,344),
      Point(425,690,689)
    )

  "Parser" - {

    "can parse the AoC example" in {
      Day08.parse(Source.fromString(example)) mustBe parsed1
    }

  }
  
  "Point" - {
    import scala.math.sqrt
    
    val points = Table[Point, Point, Double](
      ("first", "second", "distance"),
      (Point(1, 2, 3), Point(2, 3, 4), sqrt(3)),
      (Point(1, 2, 3), Point(3, 4, 5), sqrt(12)),
      (Point(1, 2, 3), Point(2, 4, 6), sqrt(14)),
    )
    
    "distanceTo is correct" in forAll(points) { (first, second, distance) =>
      first.distanceTo(second) mustBe distance
    }
    
  }


  "Part 1" - {

    "can calculate AoC example" in {
      Day08.part1(parsed1, 10) mustBe 40
    }

  }

  "Part 2" - {

    "can calculate AoC example" in {
      
    }

  }


}
