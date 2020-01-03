package parser

import scala.util.parsing.input.Positional

/**
  * Created by Greeting on 2017/3/10.
  *
  * 不用标this，直接把它当个变量，在后面语义分析的时候来处理
  */
sealed abstract class TinyAST extends Positional{
  def walk(deep:Int):Unit
}

/**
  * 只是用来接收一下id类型的String，暂存一下，不能出现在最后的AST中
  * @param name
  */
case class NodeAcceptName(name:String) extends TinyAST{
  override def walk(deep: Int) = {
    println("this is a temporary node should be removed")
  }
}

case class NodeNext(current:TinyAST, next:TinyAST) extends TinyAST{
  override def walk(deep:Int) = {
    if(current != null){
      current.walk(deep)
    }
    if(next != null){
      next.walk(deep)
    }
  }
}

case class NodeBlock(block:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Block Begin")
    block.walk(deep+1)
    println(space+"Block End")
  }
}

case class NodeInt(value:Int) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Int : "+value)
  }
}

case class NodeReal(value:Double) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Real : "+value)
  }
}

case class NodeBool(value:Boolean) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Real : "+value)
  }
}

case class NodeString(value:String) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "String : "+value)
  }
}

case class NodeVariable(name:String) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Var : "+name)
  }
}


case class NodeVariableDeclaration(isStatic:Boolean,varType:String,varName:String) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Variable Define Begin")
    println(space + "Variable isStatic : " + isStatic)
    println(space + "Variable Type : " + varType)
    println(space + "Variable Name : " + varName)
    println(space + "Variable Define End")
  }
}

case class NodeAssign(varName:String,varValue:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Variable Assign Begin")
    println(space + "Variable Name : " + varName)
    varValue.walk(deep+1)
    println(space + "Variable Assign End")
  }
}



case class NodeFunctionDefine(isStatic:Boolean,returnType:String,name:String,params:TinyAST,block:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Function Define Begin")
    println(space + "Function isStatic : " + isStatic)
    println(space + "Function ReturnType : " + returnType)
    println(space + "Function Name : " + name)
    params.walk(deep+1)
    block.walk(deep+1)
    println(space + "Function Define End")
  }
}

case class NodeFunctionCall(name:String,params:Option[TinyAST]) extends TinyAST{
  override def walk(deep:Int) = {
    val space = " " * deep
    println(space + "Function Call Begin")
    println(space + "Function Name : " + name)
    params match{
      case None => println(space + "No Params")
      case Some(p) => p.walk(deep+1)
    }
    println(space + "Function Call End")
  }
}


/**
  * for the comma
  * like a,b,c
  */
case class NodeSequence(nodes:List[TinyAST]) extends TinyAST{
  override def walk(deep: Int) = {
    nodes.foreach(_.walk(deep))
  }
}

case class NodeReturn(value:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Return Begin")
    value.walk(deep+1)
    println(space+"Return End")
  }
}


case class NodeFullCondition(ifCondition:TinyAST,elseCondition:Option[TinyAST],elseIfCondition:TinyAST*) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep

    println(space+"If Begin")
    ifCondition.walk(deep+1)
    println(space+"If End")

    if(elseIfCondition.length  != 0) {
      println(space + "ElseIf Begin")
      elseIfCondition.foreach(_.walk(deep + 1))
      println(space + "ElseIf End")
    }

    if(elseCondition != null) {
      println(space + "Else Begin")
      elseCondition match {
        case None => Unit
        case Some(elseCondition) => elseCondition.walk(deep+1)
      }
      println(space + "Else End")
    }


  }
}

case class NodeCondition(condition:Option[TinyAST],thenBlock:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Simple Condition Begin")
    condition match {
      case None => Unit
      case Some(condition) => condition.walk(deep+1)
    }
    thenBlock.walk(deep+1)
    println(space+"Simple Condition End")
  }
}



case class NodeWhile(condition:TinyAST,block:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"While Begin")
    condition.walk(deep+1)
    block.walk(deep+1)
    println(space+"While End")
  }
}

case class NodeDoWhile(doBlock:TinyAST,condition:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"DoWhile Begin")
    doBlock.walk(deep+1)
    condition.walk(deep+1)
    println(space+"DoWhile End")
  }
}

case class NodeFor(init:Option[TinyAST],condition:Option[TinyAST],repeat:Option[TinyAST]) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"For Begin")
    init match {
      case None => Unit
      case Some(init) => init.walk(deep+1)
    }
    condition match {
      case None => Unit
      case Some(condition) => condition.walk(deep+1)
    }
    repeat match {
      case None => Unit
      case Some(repeat) => repeat.walk(deep+1)
    }
    println(space+"For End")
  }
}


case class NodeBreak() extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Break")
  }
}

case class NodeContinue() extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space + "Continue")
  }
}

case class NodeClassDefine(name:String,block:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Class Define Begin")
    println(space+"Class Name : "+name)
    block.walk(deep+1)
    println(space+"Class Define End")
  }
}


case class NodeExpr(inner:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Expr Begin")
    inner.walk(deep+1)
    println(space+"Expr End")
  }
}

case class NodeRound(expr:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Round Begin")
    expr.walk(deep+1)
    println(space+"Round End")
  }
}

case class NodeIndex(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Index Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Index End")
  }
}


case class NodeAdd(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Add Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Add End")
  }
}

case class NodeSub(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Sub Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Sub End")
  }
}

case class NodeMul(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Mul Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Mul End")
  }
}

case class NodeDiv(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Div Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Div End")
  }
}

case class NodeMod(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Mod Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Mod End")
  }
}

case class NodeGt(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Gt Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Gt End")
  }
}


case class NodeGte(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Gte Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Gte End")
  }
}

case class NodeLt(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Lt Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Lt End")
  }
}

case class NodeLte(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Lte Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Lte End")
  }
}

case class NodeEq(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Eq Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Eq End")
  }
}


case class NodeNeq(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Neq Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Neq End")
  }
}

case class NodeNot(target:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Not Begin")
    target.walk(deep+1)
    println(space+"Not End")
  }
}

case class NodeDot(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"Dot Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"Dot End")
  }
}

case class NodeBitAnd(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"BitAnd Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"BitAnd End")
  }
}

case class NodeBitOr(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"BitOr Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"BitOr End")
  }
}

case class NodeLogicAnd(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"LogicAnd Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"LogicAnd End")
  }
}

case class NodeLogicOr(left:TinyAST,right:TinyAST) extends TinyAST{
  override def walk(deep: Int) = {
    val space = " " * deep
    println(space+"LogicOr Begin")
    left.walk(deep+1)
    right.walk(deep+1)
    println(space+"LogicOr End")
  }
}

