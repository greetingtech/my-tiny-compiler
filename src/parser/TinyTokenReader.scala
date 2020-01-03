package parser

import tokenizer.TinyToken

import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
  * Created by Greeting on 2017/3/11.
  */
class TinyTokenReader(tokens: Seq[TinyToken]) extends Reader[TinyToken]{
  override def first: TinyToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
  override def rest: Reader[TinyToken] = new TinyTokenReader(tokens.tail)
}
