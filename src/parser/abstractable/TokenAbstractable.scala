package parser.abstractable

import tokenizer._

import scala.util.parsing.combinator.Parsers

/**
  * Created by Greeting on 2017/3/11.
  */
trait TokenAbstractable extends Parsers {
  override type Elem = TinyToken
}
