val input = scala.io.Source.fromFile(args(0)).mkString
val parseRes = bh.bicat.parser.Parser.parse2CellGraph(input)
if( !parseRes.successful ) { println(parseRes); exit(-1) }
println(parseRes.get.toDotString)