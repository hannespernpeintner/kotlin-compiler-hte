package ast

import de.hanno.kotlin.KotlinParser._
import de.hanno.kotlin.{KotlinParser, KotlinParserBaseListener}
import lexerparser.{KotlinFileTreeWalker, LexerParser}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

class AstCreator extends KotlinParserBaseListener {

  def create(sourceCode: String): List[Instruction] = {
    create(new LexerParser().read(sourceCode))
  }

  def create(file: KotlinFileContext): List[Instruction] = {

    val instructions = new ListBuffer[Statement]()

    val stack = new mutable.Stack[Instruction]

    def isTopLevel = stack.isEmpty

    new KotlinFileTreeWalker(file).walk(new KotlinParserBaseListener() {
      override def enterFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
        val isClassMember = ctx.getParent.getParent.isInstanceOf[ClassMemberDeclarationContext]
        if(!isClassMember) return
        enterDeclareFunction(instructions, stack, isTopLevel _, ctx)
      }

      override def exitFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
        val isClassMember = ctx.getParent.getParent.isInstanceOf[ClassMemberDeclarationContext]
        if(!isClassMember) return
        exitDeclareFunction(stack)
      }

      override def enterFunctionBody(ctx: KotlinParser.FunctionBodyContext): Unit = {
        val isClassMember = !ctx.getParent.getParent.isInstanceOf[DeclarationContext]
        if(!isClassMember) return
        stack.headOption.map(_.asInstanceOf[Function]).foreach(_.bodyType = FunctionBodyType(ctx))
      }
      override def exitFunctionBody(ctx: KotlinParser.FunctionBodyContext): Unit = {
        val isClassMember = !ctx.getParent.getParent.isInstanceOf[DeclarationContext]
        if(!isClassMember) return
        stack.pop
      }

      override def enterClassDeclaration(ctx: KotlinParser.ClassDeclarationContext): Unit = {
        val clazz = Clazz(ctx.simpleIdentifier().getText, stack.headOption)
        stack.headOption.map(_.children += clazz)
        if(isTopLevel) instructions += clazz
        stack.push(clazz)
      }

      override def exitClassDeclaration(ctx: KotlinParser.ClassDeclarationContext): Unit = {
        stack.pop
      }

      override def enterPropertyDeclaration(ctx: KotlinParser.PropertyDeclarationContext): Unit = {
        val declarationContext = ctx.variableDeclaration()
        val property = Property(declarationContext.simpleIdentifier().getText, stack.headOption)
        stack.headOption.map(_.children += property)
        if(isTopLevel) instructions += property
        stack.push(property)
      }

      override def exitPropertyDeclaration(ctx: KotlinParser.PropertyDeclarationContext): Unit = {
        stack.pop
      }

      override def enterExpression(ctx: KotlinParser.ExpressionContext): Unit = {
        stack.headOption.map(it => it.children += ExpressionType(ctx, Some(it)))
      }

      override def exitExpression(ctx: KotlinParser.ExpressionContext): Unit = {
      }

      override def enterStatement(ctx: KotlinParser.StatementContext): Unit = {
        Option(ctx.declaration()).map(_.functionDeclaration()).map { it =>
          enterDeclareFunction(instructions, stack, isTopLevel _, it)
        }
      }
      override def exitStatement(ctx: KotlinParser.StatementContext): Unit = {
        Option(ctx.declaration()).map(_.functionDeclaration()).map { it =>
          exitDeclareFunction(stack)
        }
      }

    })

    instructions.toList
  }

  private def exitDeclareFunction(stack: mutable.Stack[Instruction]) = {
    stack.pop()
  }

  private def enterDeclareFunction(instructions: ListBuffer[Statement], stack: mutable.Stack[Instruction], isTopLevel: () => Boolean, ctx: FunctionDeclarationContext) = {
    val function = Function(name = ctx.simpleIdentifier().getText, parent = stack.headOption)
    stack.headOption.map(_.children += function)
    if (isTopLevel()) instructions += function
    stack.push(function)
  }
}

sealed trait Instruction {
  val parent: Option[Instruction]
  val children: mutable.MutableList[Instruction] = new mutable.MutableList[Instruction]
}
sealed trait Statement extends Instruction
sealed trait Expression extends Statement

sealed trait Declaration extends Statement
case class Function(val name: String, var bodyType: FunctionBodyType = RegularFunctionBody, override val parent: Option[Instruction]) extends Declaration
case class Clazz(name: String, override val parent: Option[Instruction]) extends Declaration
case class Property(name: String, override val parent: Option[Instruction]) extends Declaration

sealed trait Call extends Expression
case class FunctionCall(override val parent: Option[Instruction]) extends Call

case class IntExpression(val value: Int, override val parent: Option[Instruction]) extends Expression

abstract sealed class FunctionBody {
  val body: List[Instruction]
}

// TODO: I want this nested, why doesn't it work!?
sealed trait FunctionBodyType
case object ExpressionBody extends FunctionBodyType
case object RegularFunctionBody extends FunctionBodyType


object ExpressionType {
  def apply(ctx: ExpressionContext, parent: Option[Instruction]): Expression = {
    val text = ctx.getText
    if(text.endsWith(")")) {
      FunctionCall(parent)
    } else {
      val triedInt = Try(text.toInt)
      if(triedInt.isSuccess) {
        IntExpression(triedInt.get, parent)
      } else throw new IllegalStateException(s"Cannot use $text")
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