package bh.bicat.apps

import bh.bicat.parser.Parser

object App {
  def main(args : Array[String]) : Unit = {
    println("parser app start")

//    println( Parser.parse2CellGraph("""
//
//graph {
//A
//B
//C
//f: A->B
//g: B->C
//h: A->C
//k: A->C
//
//f g => h;
//
//}
//
//
//
//""") )


    println( Parser.parse2CellGraph("""

graph {

f: A->B
g: B->C
h: A->C
k: A->E

f g => h;

}



""") )

    println("parser app end")
  }
}