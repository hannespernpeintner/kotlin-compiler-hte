import KotlinParser.KotlinFileContext
import Main.code
import org.antlr.v4.runtime.tree.{AbstractParseTreeVisitor, ParseTree, ParseTreeWalker}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

object Main {

  val code =
"""package foo

fun bar() {
    // Print hello
    println("Hello, World!")
}

fun baz() = println("Hello, again!")
"""

  def main(args: Array[String]) {

    val file = new LexerParser().read(code)

    new KotlinFileTreeWalker(file).walk(new KotlinParserBaseListener() {
      override def enterFunctionDeclaration(ctx: KotlinParser.FunctionDeclarationContext): Unit = {
        println(ctx.simpleIdentifier().getText)
      }
    })

  }
}

class KotlinFileTreeWalker(file: KotlinFileContext) {
  def walk(listener: KotlinParserBaseListener): Unit = {
    new ParseTreeWalker().walk(listener, file)
  }
}

class LexerParser {
  def read(sourceCode: String): KotlinFileContext = {

    val lexer = new KotlinLexer(CharStreams.fromString(code))
    val tokens = new CommonTokenStream(lexer)
    val parser = new KotlinParser(tokens)

    parser.kotlinFile()
  }
}

class Visitor extends AbstractParseTreeVisitor[Visitor] {
  override def visit(tree: ParseTree): Visitor = {
    println(tree)
    super.visit(tree)
  }
}
