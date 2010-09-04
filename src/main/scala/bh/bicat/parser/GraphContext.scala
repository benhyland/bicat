package bh.bicat.parser

import bh.bicat.graph.{ZeroCell, OneCell, TwoCell}

// class to check for contextual failures when bound to during a parse
// does not check for failures involving a complete graph - this is done by the Graph production (but still before the parser returns a result)
class GraphContext(val zerocells: Set[ZeroCell], val onecells: Set[OneCell], val twocells: Set[TwoCell]) {
  def + (zc: ZeroCell) = new GraphContext(zerocells + zc, onecells, twocells)
  def + (oc: OneCell) = new GraphContext(zerocells, onecells + oc, twocells)
  def + (tc: TwoCell) = new GraphContext(zerocells, onecells, twocells + tc)

  def check(zc: ZeroCell) : Option[String] =
    errorIdAlreadyDefined(zc)

  def check(oc: OneCell) : Option[String] =
    errorIdAlreadyDefined(oc) orElse
    errorTailHeadEquality(oc)

  def check(tc: TwoCell) : Option[String] =
    errorItemAlreadyDefined(tc) orElse
    errorOneCellNotDefined(tc) orElse
    errorSourceSinkIdentity(tc) orElse
    errorUnconnectedPaths(tc) orElse
    errorTailHeadIdentity(tc)

  private def errorIdAlreadyDefined(zc: ZeroCell) =
    if(zerocells.contains(zc)) Some("0-cell with id '%s' is already defined".format(zc.id))
    else None

  private def errorIdAlreadyDefined(oc: OneCell) =
    if(onecells.exists( x => x match {
      case OneCell(oc.id, _, _) => true
      case _ => false
    }))
      Some("1-cell with id '%s' is already defined".format(oc.id))
    else None

  private def errorItemAlreadyDefined(tc: TwoCell) =
    if(twocells.contains(tc)) Some("2-cell similar to %s is already defined".format(tc))
    else None

  private def errorTailHeadEquality(oc: OneCell) =
    if( oc.tail == oc.head ) Some("%s has tail == head".format(oc) )
    else None

  private def findOC(id: String) = onecells.find( { case OneCell(`id`, _, _) => true; case _ => false } )

  private def errorOneCellNotDefined(tc: TwoCell) = {
    val unboundIds = for( id <- (tc.tail.edges ++ tc.head.edges) if findOC(id).isEmpty ) yield id
    if( unboundIds.isEmpty ) None
    else Some("Undefined 1-cells (%s) found in %s".format(unboundIds.mkString(", "),tc))
  }

  private def errorSourceSinkIdentity(tc: TwoCell) = {
    val (tailSource, tailSink) = (findOC(tc.tail.edges.head).get.tail, findOC(tc.tail.edges.last).get.head)
    val (headSource, headSink) = (findOC(tc.head.edges.head).get.tail, findOC(tc.head.edges.last).get.head)
    if(tailSource != headSource) Some("Sources %s, %s do not match in %s".format(tailSource, headSource, tc))
    else if(tailSink != headSink) Some("Sinks %s, %s do not match in %s".format(tailSink, headSink, tc))
    else None
  }

  private def errorUnconnectedPaths(tc: TwoCell) = {
    def getPairs(edges: List[String]) =
      (edges zip edges.tail) map { case(x, y) => (findOC(x).get.head, findOC(y).get.tail) }

    val tailPairs = getPairs(tc.tail.edges)
    val headPairs = getPairs(tc.head.edges)

    if( tailPairs.exists({ case(x, y) => x != y }) ) Some("Tail of %s is not well connected".format(tc))
    else if( headPairs.exists({ case(x, y) => x != y }) ) Some("Head of %s is not well connected".format(tc))
    else None
  }

  private def errorTailHeadIdentity(tc: TwoCell) =
    if( tc.tail == tc.head ) Some("%s has identical tail and head") else None
}