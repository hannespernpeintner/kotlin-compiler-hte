package ast

import java.util

import de.hanno.kotlin.KotlinParser.{FunctionBodyContext, KotlinFileContext}
import de.hanno.kotlin.{KotlinParser, KotlinParserBaseListener}
import lexerparser.{KotlinFileTreeWalker, LexerParser}
import org.antlr.v4.runtime.tree.TerminalNode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
        val function = Function(name = ctx.simpleIdentifier().getText, parent = stack.headOption)
        stack.headOption.map(_.children += function)
        if(isTopLevel) instructions += function
        stack.push(function)
      }

      override def exitFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
        stack.pop()
      }

      override def enterFunctionBody(ctx: KotlinParser.FunctionBodyContext): Unit = {
        stack.headOption.map(_.asInstanceOf[Function]).foreach(_.bodyType = FunctionBodyType(ctx))
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
    })

    instructions.toList
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

abstract sealed class FunctionBody {
  val body: List[Instruction]
}

// TODO: I want this nested, why doesn't it work!?
sealed trait FunctionBodyType
case object ExpressionBody extends FunctionBodyType
case object RegularFunctionBody extends FunctionBodyType

object FunctionBodyType {
  def apply(ctx: FunctionBodyContext): FunctionBodyType = {
    if(ctx.children.isEmpty) {
      RegularFunctionBody
    } else if(ctx.children.get(0).getText == "=") {
      ExpressionBody
    } else RegularFunctionBody
  }
}