package ast

import de.hanno.kotlin.KotlinParser.KotlinFileContext
import de.hanno.kotlin.{KotlinParser, KotlinParserBaseListener}
import lexerparser.{KotlinFileTreeWalker, LexerParser}
import org.antlr.v4.runtime.tree.ErrorNode

import scala.collection.mutable

class AstCreator extends KotlinParserBaseListener {

  def create(sourceCode: String): List[Ast]  = {
    create(new LexerParser().read(sourceCode))
  }

  def create(file: KotlinFileContext): List[Ast] = {

    val stack = new mutable.Stack[Ast]
    stack.push(File())

    def push(it: Ast): Unit = {
      stack.head.children += it
      stack.push(it)
    }

    new KotlinFileTreeWalker(file).walk(new KotlinParserBaseListener() {
      override def enterFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
          val function = Function(ctx.simpleIdentifier().getText, RegularFunctionBody, stack.headOption)
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
        val expressionType = ExpressionType(ctx.expression(), None)
        val property = Property(declarationContext.simpleIdentifier().getText, expressionType, stack.headOption)
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

      override def enterAssignableExpression(ctx: KotlinParser.AssignableExpressionContext): Unit = {
        println("XXX")
      }
      override def exitAssignment(ctx: KotlinParser.AssignmentContext): Unit = {
        println("YYY")
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
