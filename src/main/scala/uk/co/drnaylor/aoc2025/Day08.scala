package uk.co.drnaylor.aoc2025

import uk.co.drnaylor.aoc2025.Day08.Point
import uk.co.drnaylor.aoc2025.traits.AocDay

import scala.collection.mutable
import scala.io.Source
import scala.math.{pow, sqrt}
import scalax.collection.edges.{UnDiEdge, given}
import scalax.collection.mutable.Graph

import scala.annotation.tailrec

object Day08 extends AocDay[List[Point]] {

  case class Point(x: Long, y: Long, z: Long) {
    def distanceTo(other: Point): Double =
      sqrt(pow(this.x - other.x, 2) + pow(this.y - other.y, 2) + pow(this.z - other.z, 2))
      
    def isEqual(other: Point): Boolean = {
      this == other
    }
  }
  
  override val day: Int = 8
  override type P1 = Long
  override type P2 = Long

  override def parse(source: Source): List[Point] =
    source.getLines().filter(_.nonEmpty).map { point =>
      point.split(",", 3).map(_.trim.toInt).toList
    }.map {
      case x :: y :: z :: Nil =>
        Point(x, y, z)
      case _ => throw new IllegalStateException("That's not a point!")
    }.toList
  
  
  def getDistances(parsed: List[Point]): Seq[(Double, (Point, Point))] = 
    parsed.zipWithIndex.flatMap { case (pointa, idx) =>
      parsed.drop(idx + 1).map(pointb => (pointa.distanceTo(pointb), (pointa, pointb)))
    }.sortBy(_._1)
  
  // Part 1
  
  def part1(parsed: List[Point], connectionLimit: Int): Long = {
    // Get the top distances
    val distances = getDistances(parsed).take(connectionLimit).map(_._2)

    // We create a graph and then use that to find the biggest clusters in the
    // disconnected graph
    val edges = distances.map(x => x._1 ~ x._2)
    val graph = Graph.from(edges)
    val connectionMap: Map[Point, Int] =
      Map.from(distances.flatMap(x => List(x._1, x._2)).toSet.map(x => (x, -1)))

    @tailrec
    def getConnections(graph: Graph[Point, UnDiEdge[Point]], connectionMap: Map[Point, Int], sizes: List[Int]): List[Int] = {
      connectionMap.find(_._2 == -1) match {
        case None => sizes
        case Some((point, _)) =>
          val nextId = sizes.size
          val nodes = graph.find(point).get.innerNodeTraverser.toSet
          getConnections(
            graph,
            connectionMap.map((point, idx) => if nodes.exists(x => x.isEqual(point)) then (point, nextId) else (point, idx)),
            sizes.appended(nodes.size)
          )
      }
    }

    getConnections(graph, connectionMap, List.empty).sortBy(-_).take(3).product
  }
  
  override def part1(parsed: List[Point]): Long = {
    part1(parsed, 1000)
  }

  def part2(parsed: List[Point], startingLimit: Int): Long = {
    val totalNodes = parsed.length
    val primaryEdges = getDistances(parsed).map(_._2)
    val firstNode = primaryEdges.head._1

    val edges = primaryEdges.map(x => x._1 ~ x._2).iterator
    val graph = Graph.from(edges.take(startingLimit).toList)
    
    @tailrec
    def getSize(graph: Graph[Point, UnDiEdge[Point]], firstNode: Point, noOfPoints: Int, it: Iterator[UnDiEdge[Point]]): (Point, Point) = {
      // add the next edge to the graph
      val nextEdge = it.next()
      graph.add(nextEdge)
      
      if (graph.find(firstNode).get.innerNodeTraverser.toSet.size == noOfPoints) then (nextEdge._1, nextEdge._2)
      else getSize(graph, firstNode, noOfPoints, it)
    }
    
    val (first, second) = getSize(graph, firstNode, totalNodes, edges)
    first.x * second.x
  }
  
  override def part2(parsed: List[Point]): Long = {
    part2(parsed, 1000)
  }
}
