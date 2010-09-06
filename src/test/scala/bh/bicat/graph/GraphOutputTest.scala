package bh.bicat.graph

import bh.bicat.test.matcher.ResultMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import bh.bicat.parser.Parser

class GraphOutputTest extends FunSuite with ShouldMatchers with ResultMatchers {

  test("graph can write dot output") {
    val res = Parser.parse2CellGraph("testgraph { A B C f: A->B g: B->C h: A->C f g => h; }")
    res should be (successful)
    val graph = res.get

    graph.toDotString should be (
"""digraph testgraph {

rankdir=LR;
edge [shape=circle];

A
B
C

A -> B [label=f];
B -> C [label=g];
A -> C [label=h];
}"""
      )    
  }
}