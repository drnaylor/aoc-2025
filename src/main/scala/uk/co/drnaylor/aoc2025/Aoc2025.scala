package uk.co.drnaylor.aoc2025

import cats.effect.{ExitCode, IO, IOApp}

object Aoc2025 extends IOApp {

  val days = List(
    IO(Day01.runDay()),
    IO(Day02.runDay()),
    IO(Day03.runDay(onlyFirstPart = true))
  )

  def run(args: List[String]): IO[ExitCode] = {
    if (args.isEmpty) {
      // just do the
      days.last.map(_ => ExitCode.Success)
    } else if (args.contains("all")) {
      days.fold(IO.unit)((curr, next) => curr *> next).map(_ => ExitCode.Success)
    } else {
      args.map(_.toInt).map(v => days(v - 1)).fold(IO.unit)((curr, next) => curr *> next).map(_ => ExitCode.Success)
    }
  }

}
