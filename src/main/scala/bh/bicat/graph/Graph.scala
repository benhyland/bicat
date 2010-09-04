package bh.bicat.graph

import org.jgrapht.alg.CycleDetector
import org.jgrapht.DirectedGraph
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.traverse.DepthFirstIterator
import org.jgrapht.traverse.GraphIterator

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

sealed abstract class GraphItem;

case class ZeroCell(id:String) extends GraphItem {
  override def toString() = "0-cell(%s)".format(id)
}

case class OneCell(id:String, tail:ZeroCell, head:ZeroCell) extends GraphItem {
  override def toString() = "1-cell(%s: %s -> %s)".format(id, tail.id, head.id)
}

case class TwoCell(tail:Path, head:Path) extends GraphItem {
  override def toString() = "2-cell(%s => %s)".format(tail.edges.mkString(" "), head.edges.mkString(" "))
}

case class Path(edges: List[String])

class Graph(val name: String, val zerocells: Set[ZeroCell], val onecells: Set[OneCell], val twocells: Set[TwoCell]) {

  val graph : DirectedGraph[String,String] = new DefaultDirectedGraph[String, String](classOf[String]);
  for( v <- zerocells ) graph.addVertex(v.id);
  for( e <- onecells ) graph.addEdge( e.tail.id, e.head.id, e.id );
  val (sources, sinks) = getCandidates

  private def getCandidates = {
    val sources = ListBuffer.empty[String]
    val sinks = ListBuffer.empty[String]
    graph.vertexSet() foreach { v =>
      if( graph.inDegreeOf(v) == 0 ) sources += v
      if( graph.outDegreeOf(v) == 0 ) sinks += v
    }
    (sources.toList, sinks.toList)
  }

  override def toString() = {
    "[zerocells(" + zerocells.mkString(", ") +
    "), onecells(" + onecells.mkString(", ") +
    "), twocells(" + twocells.mkString(", ") + ")]"
  }

  def getError() : Option[String] = {
    errorCyclic orElse
    errorNoUniqueSourceSink orElse
    errorNoPastingScheme orElse
    None
  }

  private def errorCyclic = {
    val cd = new CycleDetector(graph)
    if( cd.detectCycles() ) Some("Graph contains a cycle")
    else None
  }

  private def errorNoUniqueSourceSink = {
    if( sources.size != 1 ) Some("No unique source found. Candidates were %s".format(sources.mkString(", ")))
    else if( sinks.size != 1 ) Some("No unique sink found. Candidates were %s".format(sinks.mkString(", ")))
    else None
  }

  private def errorNoPastingScheme = {
    val path = findPath(sources(0), sinks(0))
    println("path: " + path.mkString(", "))

    // pick any path s->t, make new graph from it, maintain tail and head for scheme
    // add any 2cell that fits to the graph
    // add means add all edges in the 2cell's paths, updating the scheme tail and head
    // fits means that some part of the head or tail of the 2cell is present on the tail or head of the scheme, respectively
    // we pass iff all 2cells are used
    None
  }

  private def findPath(source: String, sink: String) : List[String] = {
    val it : GraphIterator[String,String] = new DepthFirstIterator(graph, source)
    findPath(it, ListBuffer.empty, sink)
  }
  private def findPath(it : GraphIterator[String,String], path: ListBuffer[String], target: String) : List[String] = {
    val next = it.next()
    path += next
    if( target == next ) path.toList
    else findPath(it, path, target)
  }
}