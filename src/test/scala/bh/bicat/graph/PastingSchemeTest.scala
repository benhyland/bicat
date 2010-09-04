package bh.bicat.graph

import bh.bicat.test.matcher.ResultMatchers
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import bh.bicat.parser.Parser

class PastingSchemeTest extends FunSuite with ShouldMatchers with ResultMatchers {

  test("disallow graphs without a pasting scheme") {
    val r1 = Parser.parse2CellGraph("testgraph { f: A ->B g: B->D h: A->C k: C->D j: A -> D f g => j; h k => j; }")
    r1 should not be (successful)
    println(r1)

    val r2 = Parser.parse2CellGraph("""testgraph {
            f: A ->B
            g: B->C
            h: A->C
            k: C->D
            p: A->E
            q: E->F
            r: F->G
            s: E->G
            t: G -> D

            f g => h;
            s => q r;
            }""")
    r2 should not be (successful)
    println(r2)
  }
}