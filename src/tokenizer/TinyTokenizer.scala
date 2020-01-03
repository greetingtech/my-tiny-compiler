package tokenizer

import result.{TinyTokenizerFailure, TinyTokenizerResult, TinyTokenizerSuccess}

/**
  * Created by Greeting on 2017/3/7.
  */
object TinyTokenizer extends TokenRulable {

  def scanTokens(code:String):TinyTokenizerResult = {
    parse(tokens,code) match{
      case Success(result,input) => TinyTokenizerSuccess(result);
      case NoSuccess(message,input) => TinyTokenizerFailure(makeErrorMessage(message,input));
    }
  }

  def makeErrorMessage(message:String,input:Input):String = {
    val errorMessage = "token error : " + message + "\n" + "line : "+input.pos.line+" column : " + input.pos.column
    return errorMessage
  }

}
