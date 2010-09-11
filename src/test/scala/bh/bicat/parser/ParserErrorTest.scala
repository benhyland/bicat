package bh.bicat.parser

import bh.bicat.test.matcher.ResultMatchers
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ParserErrorTest extends FunSuite with ShouldMatchers with ResultMatchers {

  // parse errors

  test("disallow fragments") {
    val r1 = Parser.parse2CellGraphFragment("")(Parser.zerocell)
    r1 should not be (successful)
    val r2 = Parser.parse2CellGraphFragment("f")(Parser.onecell)
    r2 should not be (successful)
    val r3 = Parser.parse2CellGraphFragment("f : A")(Parser.onecell)
    r3 should not be (successful)
    val r4 = Parser.parse2CellGraphFragment("f g => ")(Parser.twocell)
    r4 should not be (successful)
  }

  test("disallow reused graphitem names") {
    val r1 = Parser.parse2CellGraphFragment("A A")(Parser.context)
    r1 should not be (successful)

    val r2 = Parser.parse2CellGraphFragment(" f: A->B g: B->C f: A->C")(Parser.context)
    r2 should not be (successful)

    val r3 = Parser.parse2CellGraphFragment("A B C f: A->B g: B->C h:A->C f g => h;")(Parser.context)
    r3 should be (successful)
  }

  test("disallow identical graphitems (with different names)") {
    // identical 1-cells are allowed
    val r1 = Parser.parse2CellGraphFragment("A B C f: A->B g: A->B")(Parser.context)
    r1 should be (successful)

    // identical 2-cells are not
    val r2 = Parser.parse2CellGraphFragment("A B C f: A->B g: B->C h: A->C f g => h; f g => h;")(Parser.context)
    r2 should not be (successful)
  }

  test("disallow onecells with identical tail and head") {
    val r1 = Parser.parse2CellGraphFragment("f: A->A")(Parser.context)
    r1 should not be (successful)
  }

  test("disallow twocells without prexisting onecells") {
    val r1 = Parser.parse2CellGraphFragment("f: A->B g: B -> C f g => h;")(Parser.context)
    r1 should not be (successful)
  }

  test("disallow twocells with non-matching source and sink") {
    val r1 = Parser.parse2CellGraphFragment("e: A->B f: B -> C g: A->C h: C->D e f => g h;")(Parser.context)
    r1 should not be (successful)
  }

  test("disallow twocells with non-connective paths") {
    val r1 = Parser.parse2CellGraphFragment("e: A->B f: B -> C g: C->D h: A->D e g => h;")(Parser.context)
    r1 should not be (successful)
  }

  test("disallow twocells with identical tail and head") {
    val r1 = Parser.parse2CellGraphFragment("f: A ->B g: B->C f g => f g;")(Parser.context)
    r1 should not be (successful)
  }

  test("disallow directed cycles") {
    val r1 = Parser.parse2CellGraph("testgraph {f: A ->B g: B->C h: A->C f g => h; p: C->D q: D->B }")
    r1 should not be (successful)
  }

  test("disallow graphs without a unique source and sink") {
    val r1 = Parser.parse2CellGraph("testgraph { f: A ->C g: B->C h: C->D p: D->E q: C->E h p => q; }")
    r1 should not be (successful)
    val r2 = Parser.parse2CellGraph("testgraph { f: A ->B g: B->C h: A->C p: C->D q: C->E f g => h; }")
    r2 should not be (successful)
    val r3 = Parser.parse2CellGraph("testgraph { X Y Z f: A ->B g: B->C h:A->C f g => h; }")
    r3 should not be (successful)
    val r4 = Parser.parse2CellGraph("""
      testgraph { f: A ->B g: B->C h:A->C f g => h;
      p: X->Y q:Y->Z r:X->Z p q => r;
      }""")
    r4 should not be (successful)
    val r5 = Parser.parse2CellGraph("""
      testgraph { f: A ->B g: B->C h:A->C f g => h;
      p: X->Y q:Y->X
      }""")
    println(r5)
    r5 should not be (successful)

  }
}