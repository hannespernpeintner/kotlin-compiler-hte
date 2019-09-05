package ast

import de.hanno.kotlin.KotlinParser._

import scala.collection.mutable

sealed trait Ast {
  val parent: Option[Ast]
  val children: mutable.MutableList[Ast] = new mutable.MutableList[Ast]
}
case class File(final override val parent: Option[Ast] = None) extends Ast

sealed trait Statement extends Ast
sealed trait Expression extends Statement

sealed trait Declaration extends Statement
case class Function(
  val name: String,
  var bodyType: FunctionBodyType = RegularFunctionBody,
  override val parent: Option[Ast]
) extends Declaration

case class Clazz(name: String, override val parent: Option[Ast]) extends Declaration
case class Property(name: String, override val parent: Option[Ast]) extends Declaration {
  if(name == null || name.isEmpty) throw new IllegalArgumentException("Empty string for property name not allowed!")
}

sealed trait Call extends Expression
case class FunctionCall(
 val receiver: Option[String],
 val name: String,
 val params: Option[String], override val parent: Option[Ast]
) extends Call

case class IntExpression(val value: Int, override val parent: Option[Ast]) extends Expression
case class StringExpression(val value: String, override val parent: Option[Ast]) extends Expression

sealed case class FunctionBody(val bodyType: FunctionBodyType, override val parent: Option[Ast]) extends Ast

// TODO: I want this nested, why doesn't it work!?
sealed trait FunctionBodyType
case object ExpressionBody extends FunctionBodyType
case object RegularFunctionBody extends FunctionBodyType


object ExpressionType {
  private val intPattern = """(^\d+$)""".r
  private val stringPattern = """"[^"]*"""".r
  private val functionCallPattern = """(\w*\.)?(\w+)\((.*)\)""".r

  def apply(ctx: ExpressionContext, parent: Option[Ast]): Expression = ctx.getText match {
    case intPattern(i) => IntExpression(i.toInt, parent)
    case stringPattern(text) => StringExpression(text, parent)
    case functionCallPattern(receiver, functionName, params) => {
      FunctionCall(Option(receiver), functionName, Option(params), parent)
    }
  }
}

object FunctionBodyType {
  def apply(ctx: FunctionBodyContext): FunctionBodyType = {
    if(ctx.children.isEmpty) {
      RegularFunctionBody
    } else if(ctx.children.get(0).getText == "=") {
      ExpressionBody
    } else RegularFunctionBody
  }
}