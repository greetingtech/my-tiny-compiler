package parser

import parser.abstractable.TokenAbstractable
import tokenizer.TinyToken

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
  * Created by Greeting on 2017/3/10.
  */
object TinyParser extends TokenAbstractable {

  def parseTokens(tokens:Seq[TinyToken]):TinyAST = {
    ???
  }

}
