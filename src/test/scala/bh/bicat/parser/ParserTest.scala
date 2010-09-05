package bh.bicat.parser

import bh.bicat.graph._
import bh.bicat.test.matcher.ResultMatchers
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ParserTest extends FunSuite with ShouldMatchers with ResultMatchers {

  val A = ZeroCell("A")
  val B = ZeroCell("B")
  val C = ZeroCell("C")

  val f = OneCell("f", A, B)
  val g = OneCell("g", B, C)
  val h = OneCell("h", A, C)

  val _f_g = Path(List("f","g"))
  val _h = Path(List("h"))

  val alpha = TwoCell(_f_g, _h)

  test("atomic zerocell") {
    val zc: ZeroCell = Parser.parse2CellGraphFragment("A")(Parser.zerocell).get
    zc should equal (A)
  }

  test("atomic onecell") {
    val oc: OneCell = Parser.parse2CellGraphFragment("f : A -> B")(Parser.onecell).get
    oc should equal (f)
  }

  test("atomic twocell") {
    val tc: TwoCell = Parser.parse2CellGraphFragment("f g => h ;")(Parser.twocell).get
    tc should equal (alpha)
  }

  test("first statement") {
    val (ctx, gi) = Parser.parse2CellGraphFragment("A")(Parser.firststatement).get
    assert(ctx.zerocells.isEmpty)
    assert(ctx.onecells.isEmpty)
    assert(ctx.twocells.isEmpty)
    gi should equal (A)
  }

  test("context binding") {
    val ctx1 = Parser.parse2CellGraphFragment("A")(Parser.bind(Parser.firststatement)).get
    assert(ctx1.zerocells(A))
    assert(ctx1.onecells.isEmpty)
    assert(ctx1.twocells.isEmpty)

    val ctx2 = Parser.parse2CellGraphFragment("f : A -> B")(Parser.bind(Parser.firststatement)).get
    assert(ctx2.zerocells(A))
    assert(ctx2.zerocells(B))
    assert(ctx2.onecells(f))
    assert(ctx2.twocells.isEmpty)
  }

  test("recursive binding") {
    val r1 = Parser.parse2CellGraphFragment("f : A -> B")(Parser.context)
    r1 should be (successful)
    val ctx1 = r1.get
    assert(ctx1.zerocells(A))
    assert(ctx1.zerocells(B))
    assert(!ctx1.zerocells(C))
    assert(ctx1.onecells(f))
    assert(ctx1.twocells.isEmpty)


    val r2 = Parser.parse2CellGraphFragment("f : A -> B g: B->C h: A->C f g => h;")(Parser.context)
    r2 should be (successful)
    val ctx2 = r2.get
    ctx2.zerocells should equal (Set(A, B, C))
    ctx2.onecells should equal (Set(f, g, h))
    assert(ctx2.twocells(alpha))
  }

  test("body") {
    val ctx1 = Parser.parse2CellGraphFragment("{A}")(Parser.body).get
    assert(ctx1.zerocells(A))
    assert(ctx1.onecells.isEmpty)
    assert(ctx1.twocells.isEmpty)

    val ctx2 = Parser.parse2CellGraphFragment("{ f : A -> B g: B->C h: A->C f g => h; }")(Parser.body).get
    ctx2.zerocells should equal (Set(A, B, C))
    ctx2.onecells should equal (Set(f, g, h))
    assert(ctx2.twocells(alpha))
  }

  test("graph") {
    val res = Parser.parse2CellGraphFragment("testgraph { f : A -> B g: B->C h: A->C f g => h; }")(Parser.graphcheck)
    res should be (successful)
    val graph = res.get
    graph.zerocells should equal (Set(A, B, C))
    graph.onecells should equal (Set(f, g, h))
    assert(graph.twocells(alpha))
    graph.name should equal ("testgraph")
    graph.getError should equal (None)
  }

  test("small valid graph") {
    val res = Parser.parse2CellGraph("testgraph { A B C f: A->B g: B->C h: A->C f g => h; }")
    res should be (successful)
    val graph = res.get 
    assert(graph.zerocells == Set(A, B, C))
    assert(graph.onecells == Set(f, g, h))
    assert(graph.twocells(alpha))
  }

  test("larger valid graph") {
    val res = Parser.parse2CellGraph(
      """
      testgraph { A B C
      f: A->B
      g: A->F
      i: B->G
      n: G->I
      j: F->H
      o: H->I

      f i n => g j o;

      h : B -> C
      l : C-> I

      h l => i n ;

      k:C->D
      m:D->E
      s: I->J
      t : J ->E
      u: E->
      N
      p:H->K q:K->L r:L->M v:M->N
      k m=>l s t
      ;o s t u => p q r v;
      }
      """)

    res should be (successful)
  }
}