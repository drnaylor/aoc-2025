package uk.co.drnaylor.aoc2025.test

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.co.drnaylor.aoc2025.Day11

import scala.io.Source

class Day11Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  val example: String =
    """aaa: you hhh
      |you: bbb ccc
      |bbb: ddd eee
      |ccc: ddd eee fff
      |ddd: ggg
      |eee: out
      |fff: out
      |ggg: out
      |hhh: ccc fff iii
      |iii: out
      |""".stripMargin

  val parsed1: Map[String, Set[String]] = Map.from(
    Seq(
      ("aaa", Set("you", "hhh")),
      ("you", Set("bbb", "ccc")),
      ("bbb", Set("ddd", "eee")),
      ("ccc", Set("ddd", "eee", "fff")),
      ("ddd", Set("ggg")),
      ("eee", Set("out")),
      ("fff", Set("out")),
      ("ggg", Set("out")),
      ("hhh", Set("ccc", "fff", "iii")),
      ("iii", Set("out"))
    )
  )

  val example2: String =
    """svr: aaa bbb
      |aaa: fft
      |fft: ccc
      |bbb: tty
      |tty: ccc
      |ccc: ddd eee
      |ddd: hub
      |hub: fff
      |eee: dac
      |dac: fff
      |fff: ggg hhh
      |ggg: out
      |hhh: out
      |""".stripMargin

  val parsed2: Map[String, Set[String]] = Map.from(
    Seq(
      ("svr", Set("aaa", "bbb")),
      ("aaa", Set("fft")),
      ("fft", Set("ccc")),
      ("bbb", Set("tty")),
      ("tty", Set("ccc")),
      ("ccc", Set("ddd", "eee")),
      ("ddd", Set("hub")),
      ("hub", Set("fff")),
      ("eee", Set("dac")),
      ("dac", Set("fff")),
      ("fff", Set("ggg", "hhh")),
      ("ggg", Set("out")),
      ("hhh", Set("out"))
    )
  )

  "Parser" - {

    "can parse the part 1 AoC example" in {
      Day11.parse(Source.fromString(example)) mustBe parsed1
    }

    "can parse the part 2 AoC example" in {
      Day11.parse(Source.fromString(example2)) mustBe parsed2
    }

  }

  "Part 1" - {

    "can calculate AoC example" in {
      Day11.part1(parsed1) mustBe 5
    }

  }

  "Part 2" - {

    "can calculate AoC example" in {
      Day11.part2(parsed2) mustBe 2
    }

  }

}
