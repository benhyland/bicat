package bh.bicat.graph

import org.jgrapht.DirectedGraph

import org.jgrapht.traverse.DepthFirstIterator
import org.jgrapht.traverse.GraphIterator

import scala.collection.mutable.ListBuffer

class Paster(val graph : DirectedGraph[String, String], val onecells: Set[OneCell], val twocells: Set[TwoCell], val s: String, val t: String) {

  lazy val path = findPath
  lazy val unpasteables =  findUnpasteables

  private def findPath : List[String] = {
    val it : GraphIterator[String,String] = new DepthFirstIterator(graph, s)
    findPath(it, ListBuffer.empty, t)
  }

  private def findPath(it : GraphIterator[String,String], path: ListBuffer[String], target: String) : List[String] = {
    val next = it.next()
    path += next
    if( target == next ) path.toList
    else findPath(it, path, target)
  }

  def hasPastingScheme : Boolean = {
    unpasteables.isEmpty
  }

  // TODO: remove inline prints when confident, tidy up unroll and suchlike

  def findUnpasteables = paste(path, path, ListBuffer(twocells.toList:_*))

  def paste(tail: List[String], head: List[String], remainingFaces: ListBuffer[TwoCell]) : List[TwoCell] = {
//    println( "tail: " + tail.mkString(", ") + " --- head: " + head.mkString(", ") + " --- rem: " + remainingFaces.map( f => "[tail:"+ unroll(f.tail.edges).mkString(",") + " - head:" + unroll(f.head.edges).mkString(",") + "]").mkString(", "))
    remainingFaces.find( f => canPaste(tail, unroll(f.head.edges)) ).map( f => paste(doPaste(tail, unroll(f.head.edges), unroll(f.tail.edges)), head, remainingFaces-=f)).getOrElse(
    remainingFaces.find( f => canPaste(head, unroll(f.tail.edges)) ).map( f => paste(tail, doPaste(head, unroll(f.tail.edges), unroll(f.head.edges)), remainingFaces-=f)).getOrElse(
    remainingFaces.toList))
  }

  def canPaste( path: List[String], facePath: List[String] ) = {
    val yes = path.containsSlice(facePath)
//    println( "canPaste " + yes + " path: " + path.mkString(", ") + " --- facePath: " + facePath.mkString(", ") )
    yes
  }
  def doPaste( path: List[String], facePath: List[String], insertion: List[String] ) = path.patch(path.indexOfSlice(facePath), insertion, facePath.length)
  def unroll( path: List[String] ) : List[String] = {
    val unrolledPath = ListBuffer.empty[String]
    for( id <- path ) {
      val oc = onecells.find(_.id == id).get
      unrolledPath += oc.tail.id
    }
    unrolledPath += onecells.find(_.id == path.last).get.head.id
    unrolledPath.toList
  }
}

object Paster {
  def apply(graph : DirectedGraph[String, String], onecells: Set[OneCell], twocells: Set[TwoCell], s: String, t: String) =
    new Paster(graph, onecells, twocells, s, t)
}
