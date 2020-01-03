package parser.abstractable

import parser._
import tokenizer._

/**
  * Created by Greeting on 2017/3/17.
  */
trait BaseAbstractable extends TokenAbstractable{

  def tempNodeId : Parser[NodeAcceptName] = positioned(
    accept("Temp Node Id",{case TokenId(name) => NodeAcceptName(name)})
  )

  def tempNodeType : Parser[NodeAcceptName] = positioned(
    accept("Temp Node Id",{case TokenId(name) => NodeAcceptName(name)})
  )

  def nodeInt : Parser[TinyAST] = positioned(
    accept("Node Int",{case TokenInt(value) => NodeInt(value)})
  )

  def nodeReal : Parser[TinyAST] = positioned(
    accept("Node Real",{case TokenReal(value) => NodeReal(value)})
  )

  def nodeBool : Parser[TinyAST] = positioned(
    accept("Node Bool",{case TokenBool(value) => NodeBool(value)})
  )

  def nodeString : Parser[TinyAST] = positioned(
    accept("Node String",{case TokenString(value) => NodeString(value)})
  )

  def nodeFunctionCall : Parser[TinyAST] = positioned(
    tempNodeId~TokenRoundLeft()~opt(nodeSequence)~TokenRoundRight() ^^ {
      case NodeAcceptName(name)~_~None~_ => NodeFunctionCall(name,None)
      case NodeAcceptName(name)~_~Some(list)~_ => NodeFunctionCall(name,Some(list))
    }
  )

  def nodeSequence : Parser[TinyAST] = positioned(
    nodeValuable~rep(TokenComma()~>nodeValuable) ^^ {
      case nodeValuable~Nil => NodeSequence(List(nodeValuable))
      case nodeValuable~list => NodeSequence(nodeValuable::list)
    }
  )

  def nodeAssign : Parser[TinyAST] = positioned(
    tempNodeId~TokenAssign()~nodeValuable ^^ {
      case tempNodeId~_~nodeValuable => NodeAssign(tempNodeId.name,nodeValuable)
    }
  )

  def nodeVariable : Parser[TinyAST] = positioned(
    tempNodeId ^^ {
      case NodeAcceptName(name) => NodeVariable(name)
    }
  )

  def nodeValuable : Parser[TinyAST] = positioned(
    nodeInt | nodeReal | nodeBool | nodeString | nodeFunctionCall | nodeVariable | nodeExpr
  )

  def nodeExpr : Parser[TinyAST] = positioned(
    nodeLogicOperation
  )

  def nodeLogicOperation : Parser[TinyAST] = positioned(
    nodeBitOperation~rep((TokenLogicAnd() | TokenLogicOr())~nodeBitOperation) ^^ {
      case operation~Nil => operation
      case operation~list => list./:(operation){
        case (x,TokenLogicAnd()~y) => NodeLogicAnd(x,y)
        case (x,TokenLogicOr()~y) => NodeLogicOr(x,y)
      }
    }
  )

  def nodeBitOperation : Parser[TinyAST] = positioned(
    nodeEqOrNeq~rep((TokenBitAnd() | TokenBitOr())~nodeEqOrNeq) ^^ {
      case operation~Nil => operation
      case operation~list => list./:(operation) {
        case (x,TokenBitAnd()~y) => NodeBitAnd(x,y)
        case (x,TokenBitOr()~y) => NodeBitOr(x,y)
      }
    }
  )

  def nodeEqOrNeq : Parser[TinyAST] = positioned(
    nodeGtOrGteOrLtOrLte~rep((TokenEq()|TokenNeq())~nodeGtOrGteOrLtOrLte) ^^ {
      case operation~Nil => operation
      case operation~list => list./:(operation){
        case (x,TokenEq()~y) => NodeEq(x,y)
        case (x,TokenNeq()~y) => NodeNeq(x,y)
      }
    }
  )

  def nodeGtOrGteOrLtOrLte : Parser[TinyAST] = positioned(
    nodeAddOrSub~rep((TokenGt() | TokenGte() | TokenLt() | TokenLte())~nodeAddOrSub) ^^ {
      case operation~Nil => operation
      case operation~list => list./:(operation){
        case (x,TokenGt()~y) => NodeGt(x,y)
        case (x,TokenGte()~y) => NodeGte(x,y)
        case (x,TokenLt()~y) => NodeLt(x,y)
        case (x,TokenLte()~y) => NodeLte(x,y)
      }
    }
  )

  def nodeAddOrSub : Parser[TinyAST] = positioned(
    nodeMulOrDivOrMod~rep((TokenAdd()|TokenSub())~nodeMulOrDivOrMod) ^^ {
      case operation~Nil => operation
      case operation~list => list./:(operation){
        case (x,TokenAdd()~y) => NodeAdd(x,y)
        case (x,TokenSub()~y) => NodeSub(x,y)
      }
    }
  )

  def nodeMulOrDivOrMod : Parser[TinyAST] = positioned(
    nodeNot~rep((TokenMul()|TokenDiv()|TokenMod())~nodeNot) ^^ {
      case operation~Nil => operation
      case operation~list => list./:(operation){
        case (x,TokenMul()~y) => NodeMul(x,y)
        case (x,TokenDiv()~y) => NodeDiv(x,y)
        case (x,TokenMod()~y) => NodeMod(x,y)
      }
    }
  )

  def nodeNot : Parser[TinyAST] = positioned(
    nodeDot | (TokenNot()~nodeValuable) ^^ {
      case nodeDot : TinyAST => nodeDot
      case TokenNot()~nodeValuable => NodeNot(nodeValuable)
    }
  )


  def nodeDot : Parser[TinyAST] = positioned(
    nodeRound~rep(TokenDot()~nodeRound) ^^ {
      case operation~Nil => operation
      case operation~list => list./:(operation){
        case (x,TokenDot()~y) => NodeDot(x,y)
      }
    }
  )

  def nodeIndex : Parser[TinyAST] = positioned(
    nodeRound~rep(TokenSquareLeft()~>nodeRound<~TokenSquareRight()) ^^ {
      case operation~Nil => operation
      case operation~list => list./:(operation){
        case (x,y) => NodeIndex(x,y)
      }
    }
  )

  def nodeRound : Parser[TinyAST] = positioned(
    nodeValuable | TokenRoundLeft()~nodeValuable~TokenRoundRight() ^^ {
      case nodeValuable:TinyAST => nodeValuable
      case TokenRoundLeft()~nodeValuable~TokenRoundRight() => NodeRound(nodeValuable)
    }
  )

}





























