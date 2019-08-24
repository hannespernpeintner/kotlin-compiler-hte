package ast

import de.hanno.kotlin.KotlinParser.KotlinFileContext
import de.hanno.kotlin.{KotlinParser, KotlinParserBaseListener}
import lexerparser.{KotlinFileTreeWalker, LexerParser}

import scala.collection.mutable.ListBuffer

trait AstNode

class AstCreator extends KotlinParserBaseListener {

  def create(sourceCode: String): List[AstNode] = {
    create(new LexerParser().read(sourceCode))
  }

  def create(file: KotlinFileContext): List[AstNode] = {

    val instructions = new ListBuffer[Instruction]()

    new KotlinFileTreeWalker(file).walk(new KotlinParserBaseListener() {
      override def enterFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
        instructions += Function(ctx.simpleIdentifier().getText)
      }
    })

    instructions.toList
  }
}

sealed trait Instruction extends AstNode {
}
sealed trait Statement extends Instruction {

}
sealed trait Expression extends Instruction {
}

sealed trait Declaration extends Expression {
}
case class Function(name: String) extends Declaration
class Class extends Declaration
