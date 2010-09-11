package bh.bicat.parser

import bh.bicat.graph._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
// todo: move parser to class+companion
object Parser extends StandardTokenParsers {

  // TODO: extend dsl for labeling, and for equational analysis/proof assistance -
  // given a current graph, a target graph and a set of equalities on candidate subgraphs,
  // determine a manipulation path that leads from current to target
  // might need a basic ui for that

  // TODO: algorithm for testing isomorphism between schemes:
  // check for isomorphism between source, sink, tail, head of scheme, then
  // given some pasting on one scheme check that an isomorphic pasting exists on the other

  implicit def graphParser: Parser[Graph] = phrase(graphcheck)

  def parse2CellGraph(in: String) : ParseResult[Graph] = parse2CellGraphFragment(in)

  def parse2CellGraphFragment[T](in: String)(implicit parser: Parser[T]) : ParseResult[T] =
    parser(new lexical.Scanner(in))

  lexical.delimiters += ( "{", "}", ":", "->", "=>", ";" )

  def graphcheck: Parser[Graph] = graph >> { g => g.getError().map( failure(_) ).getOrElse( success(g) ) }

  def graph: Parser[Graph] = ident ~ body ^^ { case name ~ context => new Graph(name, context.zerocells, context.onecells, context.twocells) }

  def body: Parser[GraphContext] = "{" ~> context <~ "}"

  def context: Parser[GraphContext] = check(firststatement, statement)

  def bind(p: => Parser[(GraphContext, GraphItem)]) : Parser[GraphContext] = {
    p >> {
      case (ctx, gi) => bindContext(ctx, gi)
    }
  }

  def bindContext(ctx: GraphContext, gi: GraphItem) : Parser[GraphContext] = { gi match {
      case zc: ZeroCell => ctx.check(zc).map( failure(_) ).getOrElse( success(ctx + zc) )
      case oc @ OneCell(id,t,h) => ctx.check(oc).map( failure(_) ).getOrElse( success(ctx + t + h + oc) )
      case tc: TwoCell => ctx.check(tc).map( failure(_) ).getOrElse( success(ctx + tc) )
    }
  }

  def check(first: => Parser[(GraphContext, GraphItem)], next: => Parser[GraphItem]) : Parser[GraphContext] = {
    bind(first) ~ (next?) >> {
      case ctx ~ Some(gi) => check(success((ctx,gi)), next)
      case ctx ~ None => success(ctx)
    }
  }
  
  lazy val firststatement: Parser[(GraphContext, GraphItem)] = statement ^^ {
    case gi => (new GraphContext(Set.empty, Set.empty, Set.empty), gi)
  }

  lazy val statement: Parser[GraphItem] = (twocell | onecell | zerocell)

  lazy val zerocell: Parser[ZeroCell] = ident ^^ { case id => ZeroCell(id) }

  lazy val onecell: Parser[OneCell] = ident ~ ":" ~ ident ~ "->" ~ ident ^^ { case id ~ ":" ~ tail ~ "->" ~ head => OneCell(id, ZeroCell(tail), ZeroCell(head)) }

  lazy val twocell: Parser[TwoCell] = path ~ "=>" ~ path <~ ";" ^^ { case tail ~ "=>" ~ head => TwoCell(tail,head) }

  lazy val path: Parser[Path] = (ident+) ^^ { case edges => new Path(edges) }
}