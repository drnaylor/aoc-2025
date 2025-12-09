package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.io.Source

object Day06 extends AocDay[Seq[String]] {

  object Operations {
    sealed trait Operation {
      def initialValue: Long
      def operate(first: Long, second: Long): Long
    }

    case object Add extends Operation {
      val initialValue = 0L
      def operate(first: Long, second: Long): Long = first + second
    }

    case object Multiply extends Operation {
      val initialValue = 1L
      def operate(first: Long, second: Long): Long = first * second
    }
  }

  case class Column(values: Seq[Long], operation: Operations.Operation)

  override val day: Int = 6
  override type P1 = Long
  override type P2 = Long

  def parseNumericRow(row: String): Seq[Long] = row.split("""\s+""").flatMap(_.toLongOption).toSeq
  def parseOperatorRow(row: String): Seq[Operations.Operation] = row.split("""\s+""").map {
    case "+" => Operations.Add
    case "*" => Operations.Multiply
    case _ => throw new IllegalStateException("Must only be + or *")
  }.toSeq

  override def parse(source: Source): Seq[String] = source.getLines().filter(_.nonEmpty).toSeq

  val parseOne: PartialFunction[Seq[String], Seq[Column]] = {
    case list :+ last =>
      list.map(parseNumericRow)
        .transpose
        .zip(parseOperatorRow(last))
        .map {
          case (numbers, operation) => Column(numbers, operation)
        }
    case _ => throw new IllegalArgumentException("Should not happen")
  }
  
  val parseTwo: PartialFunction[Seq[String], Seq[Column]] = {
    case list :+ last =>
      // We take the strings, get the length and pad all strings to be the same length
      // We then transpose them, then we can separate each calculation by blank lines
      val padTo = list.map(_.length).max
      val (first, second) = list
        .map(_.padTo(padTo, ' ').toSeq)
        .transpose
        .map(_.mkString.trim.toLongOption)
        .foldLeft((Seq.empty[Seq[Long]], Seq.empty[Long])) {
          case ((completedSeq, currentSeq), Some(result)) => (completedSeq, currentSeq.appended(result))
          case ((completedSeq, currentSeq), None) => (completedSeq.appended(currentSeq), Seq.empty[Long])
        }

      val values = if second.nonEmpty then first.appended(second) else first
      values
        .zip(parseOperatorRow(last))
        .map {
          case (numbers, operation) => Column(numbers, operation)
        }
    case _ => throw new IllegalArgumentException("Should not happen")
  }

  override def part1(parsed: Seq[String]): Long =
    completeOperation(parseOne(parsed))

  def completeOperation(parsed: Seq[Column]): Long =
    parsed.map(x => x.values.foldLeft(x.operation.initialValue) { (current, next) =>
      x.operation.operate(current, next)
    }).sum

  override def part2(parsed: Seq[String]): Long =
    completeOperation(parseTwo(parsed))
}
