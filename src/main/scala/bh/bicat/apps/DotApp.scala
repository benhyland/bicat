package bh.bicat.apps

import bh.bicat.parser.Parser

object DotApp {

  def main(args : Array[String]) : Unit = {
    println("dotapp start")

    println("small dot output:")

    println(Parser.parse2CellGraph("testgraph { A B C f: A->B g: B->C h: A->C f g => h; }").get.toDotString)

    println("larger dot output:")
    println(Parser.parse2CellGraph(
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
      """).get.toDotString)

    println("dotapp end")
  }

}