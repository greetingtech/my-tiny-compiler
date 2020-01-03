package result

import exception.TinyTokenException
import tokenizer.TinyToken

/**
  * Created by Greeting on 2017/3/8.
  */
sealed trait TinyCompileResult
sealed trait TinyTokenizerResult extends TinyCompileResult{
  type resultType
  val result : resultType
  def get : resultType = this.result
}
case class TinyTokenizerSuccess(result:List[TinyToken]) extends TinyTokenizerResult{
  override type resultType = List[TinyToken]
}
case class TinyTokenizerFailure(result:String) extends TinyTokenizerResult{
  override type resultType = String
}

