package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.Day11.PathCounts.EMPTY
import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends AocDay[Map[String, Set[String]]] {

  override val day: Int = 11
  override type P1 = Int
  override type P2 = Long

  override def parse(source: Source): Map[String, Set[String]] = {
    source.getLines().filterNot(_.isEmpty).map { input =>
      val Array(in, out) = input.split(":", 2)
      (in.trim, out.trim.split(" ").map(_.trim).toSet)
    }.toMap
  }

  override def part1(parsed: Map[String, Set[String]]): Int = {

    @tailrec
    def traverseTreeForOut(inputs: Map[String, (Set[String], Int)], leftToProcess: Map[String, Set[String]]): Int = {
      val newNodes = leftToProcess.filterNot { (node, _) =>
          // Find all nodes that do not appear in the left to process values
          // this means there are unresolved dependencies we need to consider later
          leftToProcess.values.flatten.exists(x => x == node)
        }
        .map { (node, leaves) =>
          // find all inputs for this node
          val inPaths = inputs.flatMap {
            case (_, (deps, weight)) if deps.contains(node) => Some(weight)
            case _ => None
          }.sum
          (node, (leaves, inPaths)) // get the sum of the nodes above it to get how many paths traverse down
        }

      if newNodes.isEmpty then throw new IllegalStateException("Nope")

      if (newNodes.contains("out")) {
        newNodes("out")._2
      } else {
        traverseTreeForOut(inputs ++ newNodes, leftToProcess.filterNot((key, _) => newNodes.contains(key)))
      }
    }

    traverseTreeForOut(
      Map.from(Seq(("you", (parsed("you"), 1)))),
      parsed.filterNot((key, _) => key == "you") ++ Map.from(Some(("out", Set.empty))) // add empty out so it can be grabbed.
    )

  }

  object PathCounts {
    val EMPTY: PathCounts = PathCounts(false, false, 0)
  }
  case class PathCounts(visitedDac: Boolean, visitedFft: Boolean, paths: Long) {
    lazy val oneVisited: Boolean = visitedDac || visitedFft
    lazy val bothVisited: Boolean = visitedDac && visitedFft

    def visitNode(node: String): PathCounts = node match {
      case "dac" => this.copy(visitedDac = true)
      case "fft" => this.copy(visitedFft = true)
      case _ => this
    }

    def combine(that: PathCounts): PathCounts = {
      // If the statuses are the same, then combine -- easy!
      if this.visitedDac == that.visitedDac && this.visitedFft == that.visitedFft then this.copy(paths = this.paths + that.paths)
      // if one has visited dac and one has visited fft, that means neither will visit the other
      // so we go to zero.
      else if this.visitedDac == that.visitedFft && this.visitedFft == that.visitedDac && this.visitedDac != this.visitedFft then EMPTY
      // if one visited both, but the other didn't, take the one that visited both, because we'll not visit both again
      // similar story for if one is visited the other hasn't visited it
      else if (this.bothVisited && !that.bothVisited) || (this.oneVisited && !that.oneVisited) then this
      // otherwise, it's that (reverse of condition above)
      else that
    }
  }

  override def part2(parsed: Map[String, Set[String]]): Long = {

    @tailrec
    def traverseTreeForOut(inputs: Map[String, (Set[String], PathCounts)], leftToProcess: Map[String, Set[String]]): Long = {
      val newNodes = leftToProcess.filterNot { (node, _) =>
          // Find all nodes that do not appear in the left to process values
          // this means there are unresolved dependencies we need to consider later
          leftToProcess.values.flatten.exists(x => x == node)
        }
        .map { (node, leaves) =>
          // find all inputs for this node
          val inPaths = inputs.flatMap {
            case (_, (deps, weight)) if deps.contains(node) => Some(weight.visitNode(node))
            case _ => None
          }.reduceLeft((l, r) => l.combine(r))

          (node, (leaves, inPaths)) // get the sum of the nodes above it to get how many paths traverse down
        }

      if newNodes.isEmpty then throw new IllegalStateException("Nope")

      if (newNodes.contains("out")) {
        newNodes("out")._2.paths
      } else {
        traverseTreeForOut(inputs ++ newNodes, leftToProcess.filterNot((key, _) => newNodes.contains(key)))
      }
    }

    traverseTreeForOut(
      Map.from(Seq(("svr", (parsed("svr"), PathCounts(false, false, 1))))),
      parsed.filterNot((key, _) => key == "svr") ++ Map.from(Some(("out", Set.empty))) // add empty out so it can be grabbed.
    )
  }
}
