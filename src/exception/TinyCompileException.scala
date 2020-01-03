package exception

/**
  * Created by Greeting on 2017/3/8.
  */
class TinyCompileException(message: String = "") extends Exception(message)
case class TinyTokenException(message: String = "") extends TinyCompileException(message)
case class TinyParseException(message: String = "") extends TinyCompileException(message)



