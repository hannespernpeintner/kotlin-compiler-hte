package ast

import de.hanno.kotlin.KotlinParser.KotlinFileContext
import de.hanno.kotlin.{KotlinParser, KotlinParserBaseListener}
import lexerparser.{KotlinFileTreeWalker, LexerParser}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class AstCreator extends KotlinParserBaseListener {

  def create(sourceCode: String): List[AstNode] = {
    create(new LexerParser().read(sourceCode))
  }

  def create(file: KotlinFileContext): List[AstNode] = {

    val instructions = new ListBuffer[Instruction]()

    val stack = new mutable.Stack[Instruction]

    def isTopLevel = stack.isEmpty

    new KotlinFileTreeWalker(file).walk(new KotlinParserBaseListener() {
      override def enterFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
        val function = Function(ctx.simpleIdentifier().getText, stack.lastOption)
        stack.lastOption.map(_.children += function)
        if(isTopLevel) instructions += function
        stack.push(function)
      }

      override def exitFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
        stack.pop()
      }

      override def enterClassDeclaration(ctx: KotlinParser.ClassDeclarationContext): Unit = {
        val clazz = Clazz(ctx.simpleIdentifier().getText, stack.lastOption)
        stack.lastOption.map(_.children += clazz)
        if(isTopLevel) instructions += clazz
        stack.push(clazz)
      }

      override def exitClassDeclaration(ctx: KotlinParser.ClassDeclarationContext): Unit = {
        stack.pop
      }

      override def enterPropertyDeclaration(ctx: KotlinParser.PropertyDeclarationContext): Unit = {
        val declarationContext = ctx.variableDeclaration()
        val property = Property(declarationContext.simpleIdentifier().getText, stack.lastOption)
        stack.lastOption.map(_.children += property)
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

abstract class AstNode {
  val parent: Option[AstNode]
  val children: mutable.MutableList[AstNode] = new mutable.MutableList[AstNode]
}

sealed trait Instruction extends AstNode {
}
sealed trait Statement extends Instruction {

}
sealed trait Expression extends Instruction {
}

sealed trait Declaration extends Expression {
}
case class Function(name: String, override val parent: Option[AstNode]) extends Declaration
case class Clazz(name: String, override val parent: Option[AstNode]) extends Declaration
case class Property(name: String, override val parent: Option[AstNode]) extends Declaration
