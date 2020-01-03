package parser.abstractable

import parser._
import tokenizer._

/**
  * Created by Greeting on 2017/3/18.
  */
trait AdvanceAbstractable extends BaseAbstractable {

  def nodeSentences : Parser[TinyAST] = {
    rep(nodeSentence) ^^ {
      case sentences => sentences reduceRight NodeNext
    }
  }

  def nodeSentence : Parser[TinyAST] = {
    ???
  }

  def nodeBlock : Parser[TinyAST] = {
    TokenCurlyLeft()~>nodeSentences<~TokenCurlyRight() ^^ {
      case nodeSentences => NodeBlock(nodeSentences)
    }
  }

  def nodeClassDefine : Parser[TinyAST] = positioned(
    TokenClass()~tempNodeId~nodeBlock ^^ {
      case _~tempNodeId~nodeBlock => NodeClassDefine(tempNodeId.name,nodeBlock)
    }
  )

  def nodeFunctionDefine : Parser[TinyAST] = positioned(
    opt(TokenStatic())~tempNodeType~tempNodeId~TokenRoundLeft()~nodeParamsSequence~TokenRoundRight()~nodeBlock ^^ {
      case None~tempNodeType~tempNodeId~_~nodeParamsSequence~_~nodeBlock => NodeFunctionDefine(false,tempNodeType.name,tempNodeId.name,nodeParamsSequence,nodeBlock)
      case Some(_)~tempNodeType~tempNodeId~_~nodeParamsSequence~_~nodeBlock => NodeFunctionDefine(true,tempNodeType.name,tempNodeId.name,nodeParamsSequence,nodeBlock)
    }
  )

  def nodeParamsSequence : Parser[TinyAST] = {
    def nodeFunctionParams : Parser[TinyAST] = positioned(
      tempNodeType~tempNodeId ^^ {
        case tempNodeType~tempNodeId => NodeVariableDeclaration(false,tempNodeType.name,tempNodeId.name)
      }
    )
    positioned(
      nodeFunctionParams ~ rep(TokenComma() ~> nodeFunctionParams) ^^ {
        case nodeFunctionParams ~ Nil => NodeSequence(List(nodeFunctionParams))
        case nodeFunctionParams ~ list => NodeSequence(nodeFunctionParams :: list)
      }
    )
  }


  def nodeVariableDeclare : Parser[TinyAST] = positioned(
    opt(TokenStatic())~tempNodeType~tempNodeId~opt(TokenAssign()~>nodeValuable) ^^ {
      case None~tempNodeType~tempNodeId~None => NodeVariableDeclaration(false,tempNodeType.name,tempNodeId.name)
      case None~tempNodeType~tempNodeId~Some(nodeValuable) =>
        NodeNext(
          NodeVariableDeclaration(false,tempNodeType.name,tempNodeId.name),
          NodeAssign(tempNodeId.name,nodeValuable)
        )
      case Some(_)~tempNodeType~tempNodeId~None => NodeVariableDeclaration(true,tempNodeType.name,tempNodeId.name)
      case Some(_)~tempNodeType~tempNodeId~Some(nodeValuable) =>
        NodeNext(
          NodeVariableDeclaration(true,tempNodeType.name,tempNodeId.name),
          NodeAssign(tempNodeId.name,nodeValuable)
        )
    }
  )


  def nodeIf : Parser[TinyAST] = {
    def nodeCondition : Parser[TinyAST] = positioned(
      TokenRoundLeft()~opt(nodeValuable)~TokenRoundRight()~nodeBlock ^^ {
        case _~Some(nodeValuable)~_~nodeBlock => NodeCondition(Some(nodeValuable),nodeBlock)
        case _~None~_~nodeBlock => NodeCondition(None,nodeBlock)
      }
    )
    positioned(
      TokenIf()~>nodeCondition~opt(rep(TokenElseIf()~>nodeCondition))~opt(TokenElse()~>nodeCondition) ^^ {
        case ifCondition~None~None => NodeFullCondition(ifCondition,None,Nil)
        case ifCondition~Some(elseIfConditions)~None => NodeFullCondition(ifCondition,None,elseIfConditions)
        case ifCondition~None~Some(elseCondition) => NodeFullCondition(ifCondition,Some(elseCondition),Nil)
        case ifCondition~Some(elseIfConditions)~Some(elseCondition) => NodeFullCondition(ifCondition,Some(elseCondition),elseIfConditions)
      }
    )
  }

  def nodeFor : Parser[TinyAST] = {
    def nodeFirstForParam : Parser[Option[TinyAST]] = positioned(
      opt(nodeVariableDeclare | nodeAssign)
    )
    def nodeSecondForParam : Parser[Option[TinyAST]] = positioned(
      opt(nodeValuable)
    )
    def nodeThirdForParam : Parser[Option[TinyAST]] = positioned(
      opt(nodeValuable)
    )
    positioned(
      TokenFor()~TokenRoundLeft()~
        nodeFirstForParam~TokenSemicolon()~
        nodeSecondForParam~TokenSemicolon()~
        nodeThirdForParam~ TokenRoundRight() ^^ {
        case _~_~nodeFirstForParam~_~nodeSecondForParam~_~nodeThirdForParam~_ =>
          NodeFor(nodeFirstForParam,nodeSecondForParam,nodeThirdForParam)
      }
    )
  }

  def nodeWhile : Parser[TinyAST] = positioned(
    ???
  )

  def nodeDoWhile : Parser[TinyAST] = positioned(
    ???
  )



}
