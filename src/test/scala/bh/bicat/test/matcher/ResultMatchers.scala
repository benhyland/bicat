package bh.bicat.test.matcher

import bh.bicat.parser.Parser
import Parser.ParseResult
import Parser.NoSuccess
import org.scalatest.matchers.{MatchResult, BeMatcher}

trait ResultMatchers {

  class SuccessMatcher[T] extends BeMatcher[ParseResult[T]] {

    def apply(left: ParseResult[T]) = MatchResult( left match {
      case ns: NoSuccess => false
      case _ => true
    },
    left.toString + " was not a success", left.toString + " was a success" )
  }
  
  val successful = new SuccessMatcher[Any]
}