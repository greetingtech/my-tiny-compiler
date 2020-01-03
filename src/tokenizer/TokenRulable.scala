package tokenizer

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Greeting on 2017/3/5.
  */
trait TokenRulable extends RegexParsers {

  override def skipWhitespace: Boolean = true
  override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def tokens: Parser[List[TinyToken]] = {
    phrase(
      rep1(
          tokenClass | tokenContinue | tokenReturn | tokenThis | tokenIf |
          tokenElseIf | tokenElse | tokenFor | tokenDo | tokenWhile |
          tokenBool | tokenString | tokenReal | tokenInt | tokenId |
          tokenAdd | tokenSub | tokenMul | tokenDiv | tokenEq | tokenAssign |
          tokenGte | tokenGt | tokenLte | tokenLt | tokenLogicAnd | tokenLogicOr |
          tokenBitAnd | tokenBitOr | tokenRoundLeft | tokenRoundRight |
          tokenSquareLeft | tokenSquareRight | tokenCurlyLeft | tokenCurlyRight |
          tokenDot | tokenComma | tokenSemicolon | tokenNeq | tokenNot | tokenMod
      )
    )
  }


  def tokenId = positioned(
    "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { id: String => TokenId(id.toString) }
  )

  def tokenInt = positioned(
    "-?\\d+".r ^^ { value: String => TokenInt(value.toInt) }
  )

  def tokenReal = positioned(
    "-?\\d+\\.\\d+".r ^^ { value: String => TokenReal(value.toDouble) }
  )

  def tokenBool = positioned(
    ("true" | "false") ^^ {
      case "true" => TokenBool(true)
      case "false" => TokenBool(false)
    }
  )

  def tokenString = positioned(
    (""""[^"]*"""".r | """'[^']*'""".r) ^^ {
      str => TokenString(str.substring(1, str.length - 1))
    }
  )

  def tokenIf = positioned(
    "if" ^^ (_ => TokenIf())
  )

  def tokenElse = positioned(
    "else" ^^ (_ => TokenElse())
  )

  def tokenElseIf = positioned(
    "else if" ^^ (_ => TokenElseIf())
  )


  def tokenFor = positioned(
    "for" ^^ (_ => TokenFor())
  )

  def tokenDo = positioned(
    "do" ^^ (_ => TokenDo())
  )

  def tokenWhile = positioned(
    "while" ^^ (_ => TokenWhile())
  )

  def tokenContinue = positioned(
    "continue" ^^ (_ => TokenContinue())
  )

  def tokenBreak = positioned(
    "break" ^^ (_ => TokenBreak())
  )

  def tokenClass = positioned(
    "class" ^^ (_ => TokenClass())
  )

  def tokenReturn = positioned(
    "return" ^^ (_ => TokenReturn())
  )

  def tokenThis = positioned(
    "this" ^^ (_ => TokenThis())
  )

  def tokenStatic = positioned(
    "this" ^^ (_ => TokenStatic())
  )

  def tokenAdd = positioned(
    "+" ^^ (_ => TokenAdd())
  )

  def tokenSub = positioned(
    "-" ^^ (_ => TokenSub())
  )

  def tokenMul = positioned(
    "*" ^^ (_ => TokenMul())
  )

  def tokenDiv = positioned(
    "/" ^^ (_ => TokenDiv())
  )

  def tokenAssign = positioned(
    "=" ^^ (_ => TokenAssign())
  )

  def tokenEq = positioned(
    "==" ^^ (_ => TokenEq())
  )

  def tokenGt = positioned(
    ">" ^^ (_ => TokenGt())
  )

  def tokenGte = positioned(
    ">=" ^^ (_ => TokenGte())
  )

  def tokenLt = positioned(
    "<" ^^ (_ => TokenLt())
  )

  def tokenLte = positioned(
    "<=" ^^ (_ => TokenLte())
  )

  def tokenLogicAnd = positioned(
    "&&" ^^ (_ => TokenLogicAnd())
  )

  def tokenLogicOr = positioned(
    "||" ^^ (_ => TokenLogicOr())
  )

  def tokenBitAnd = positioned(
    "&" ^^ (_ => TokenBitAnd())
  )

  def tokenBitOr = positioned(
    "|" ^^ (_ => TokenBitOr())
  )

  def tokenRoundLeft = positioned(
    "(" ^^ (_ => TokenRoundLeft())
  )

  def tokenRoundRight = positioned(
    ")" ^^ (_ => TokenRoundRight())
  )

  def tokenSquareLeft = positioned(
    "[" ^^ (_ => TokenSquareRight())
  )

  def tokenSquareRight = positioned(
    "]" ^^ (_ => TokenSquareRight())
  )

  def tokenCurlyLeft = positioned(
    "{" ^^ (_ => TokenCurlyLeft())
  )

  def tokenCurlyRight = positioned(
    "}" ^^ (_ => TokenCurlyRight())
  )

  def tokenDot = positioned(
    "." ^^ (_ => TokenDot())
  )

  def tokenComma = positioned(
    "," ^^ (_ => TokenComma())
  )

  def tokenSemicolon = positioned(
    ";" ^^ (_ => TokenSemicolon())
  )

  def tokenNeq = positioned(
    "!=" ^^ (_ => TokenNeq())
  )

  def tokenNot = positioned(
    "!" ^^ (_ => TokenNot())
  )

  def tokenMod = positioned(
    "%" ^^ (_ => TokenMod())
  )




}
