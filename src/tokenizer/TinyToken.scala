package tokenizer

import scala.util.parsing.input.Positional

/**
  * Created by Greeting on 2017/3/5.
  */

sealed trait TinyToken extends Positional

//keyword
case class TokenIf() extends TinyToken
case class TokenElse() extends TinyToken
case class TokenElseIf() extends TinyToken
case class TokenFor() extends TinyToken
case class TokenDo() extends TinyToken
case class TokenWhile() extends TinyToken
case class TokenContinue() extends TinyToken
case class TokenBreak() extends TinyToken
case class TokenClass() extends TinyToken
case class TokenReturn() extends TinyToken
case class TokenThis() extends TinyToken
case class TokenStatic() extends TinyToken

//id
case class TokenId(name:String) extends TinyToken

//value
case class TokenInt(value:Int) extends TinyToken
case class TokenReal(value:Double) extends TinyToken
case class TokenBool(value:Boolean) extends TinyToken
case class TokenString(value:String) extends TinyToken

//operation
case class TokenAdd() extends TinyToken
case class TokenSub() extends TinyToken
case class TokenMul() extends TinyToken
case class TokenDiv() extends TinyToken
case class TokenMod() extends TinyToken
case class TokenAssign() extends TinyToken
case class TokenNeq() extends TinyToken
case class TokenEq() extends TinyToken
case class TokenGt() extends TinyToken
case class TokenGte() extends TinyToken
case class TokenLt() extends TinyToken
case class TokenLte() extends TinyToken
case class TokenLogicAnd() extends TinyToken
case class TokenLogicOr() extends TinyToken
case class TokenBitAnd() extends TinyToken
case class TokenBitOr() extends TinyToken
case class TokenNot() extends TinyToken

//other symbol or operation
case class TokenRoundLeft() extends TinyToken
case class TokenRoundRight() extends TinyToken
case class TokenSquareLeft() extends TinyToken
case class TokenSquareRight() extends TinyToken
case class TokenCurlyLeft() extends TinyToken
case class TokenCurlyRight() extends TinyToken

case class TokenDot() extends TinyToken
case class TokenComma() extends TinyToken
case class TokenSemicolon() extends TinyToken

