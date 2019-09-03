package ast

import ast.ExpressionType.functionCallPattern
import de.hanno.kotlin.KotlinParser._
import de.hanno.kotlin.{KotlinParser, KotlinParserBaseListener}
import lexerparser.{KotlinFileTreeWalker, LexerParser}

import scala.collection.mutable
import scala.util.Try

class AstCreator extends KotlinParserBaseListener {

  def create(sourceCode: String): List[Ast] = {
    create(new LexerParser().read(sourceCode))
  }

  def create(file: KotlinFileContext): List[Ast] = {

    val stack = new mutable.Stack[Ast]
    stack.push(File())

    var insideExpression = true

    def push(it: Ast): Unit = {
      stack.head.children += it
      stack.push(it)
    }

    new KotlinFileTreeWalker(file).walk(new KotlinParserBaseListener() {
      override def enterFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
          val function = Function(name = ctx.simpleIdentifier().getText, parent = stack.headOption)
          push(function)
      }

      override def exitFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
        stack.pop()
      }

      override def enterFunctionBody(ctx: KotlinParser.FunctionBodyContext): Unit = {
        stack.head.asInstanceOf[Function].bodyType = FunctionBodyType(ctx)
      }
      override def exitFunctionBody(ctx: KotlinParser.FunctionBodyContext): Unit = {
      }

      override def enterClassDeclaration(ctx: KotlinParser.ClassDeclarationContext): Unit = {
        val clazz = Clazz(ctx.simpleIdentifier().getText, stack.headOption)
        push(clazz)
      }

      override def exitClassDeclaration(ctx: KotlinParser.ClassDeclarationContext): Unit = {
        stack.pop
      }

      override def enterPropertyDeclaration(ctx: KotlinParser.PropertyDeclarationContext): Unit = {
        val declarationContext = ctx.variableDeclaration()
        val property = Property(declarationContext.simpleIdentifier().getText, stack.headOption)
        push(property)
      }

      override def exitPropertyDeclaration(ctx: KotlinParser.PropertyDeclarationContext): Unit = {
        stack.pop
      }

      var expressionCounter = 0
      override def enterExpression(ctx: KotlinParser.ExpressionContext): Unit = {
        val expression = ExpressionType(ctx, stack.headOption)
        if(expressionCounter == 0) {
          push(expression)
          expressionCounter += 1
        } else {
          stack.head.children += expression
        }
      }

      override def exitExpression(ctx: KotlinParser.ExpressionContext): Unit = {
        expressionCounter -= 1
        if(expressionCounter == 0) {
          stack.pop
        }
      }

      override def enterStatement(ctx: KotlinParser.StatementContext): Unit = {
        if(ctx.expression() != null) return
//        Option(ctx.declaration()).map(_.functionDeclaration()).foreach { it => {
//            val function = Function(name = it.simpleIdentifier().getText, parent = stack.headOption)
//            push(function)
//          }
//        }
      }
      override def exitStatement(ctx: KotlinParser.StatementContext): Unit = {
//        Option(ctx.declaration()).map(_.functionDeclaration()).map { _ =>
//          stack.pop()
//        }
      }

    })

    stack.head.children.toList // TODO: Return file ast
  }

  }

sealed trait Ast {
  val parent: Option[Ast]
  val children: mutable.MutableList[Ast] = new mutable.MutableList[Ast]
}
case class File(final override val parent: Option[Ast] = None) extends Ast

sealed trait Statement extends Ast
sealed trait Expression extends Statement

sealed trait Declaration extends Statement
case class Function(val name: String,
                    var bodyType: FunctionBodyType = RegularFunctionBody,
                    override val parent: Option[Ast]) extends Declaration
case class Clazz(name: String, override val parent: Option[Ast]) extends Declaration
case class Property(name: String, override val parent: Option[Ast]) extends Declaration {
  if(name == null || name.isEmpty) throw new IllegalArgumentException("Empty string for property name not allowed!")
}

sealed trait Call extends Expression
case class FunctionCall(val receiver: Option[String], val name: String, val params: Option[String], override val parent: Option[Ast]) extends Call

case class IntExpression(val value: Int, override val parent: Option[Ast]) extends Expression
case class StringExpression(val value: String, override val parent: Option[Ast]) extends Expression

sealed case class FunctionBody(val bodyType: FunctionBodyType, override val parent: Option[Ast]) extends Ast

// TODO: I want this nested, why doesn't it work!?
sealed trait FunctionBodyType
case object ExpressionBody extends FunctionBodyType
case object RegularFunctionBody extends FunctionBodyType


object ExpressionType {
  val intPattern = """(^\d+$)""".r
  val stringPattern = """"[^"]*"""".r
  val functionCallPattern = """(\w*\.)?(\w+)\((.*)\)""".r

  def apply(ctx: ExpressionContext, parent: Option[Ast]): Expression = {
    val text = ctx.getText

    text match {
      case intPattern(i) => IntExpression(i.toInt, parent)
      case stringPattern() => StringExpression(text, parent)
      case functionCallPattern(receiver, functionName, params) => {
        FunctionCall(Option(receiver), functionName, Option(params), parent)
      }
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