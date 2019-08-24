import Main.code
import de.hanno.kotlin.KotlinParser.KotlinFileContext
import de.hanno.kotlin.{KotlinParser, KotlinParserBaseListener}
import lexerparser.{KotlinFileTreeWalker, LexerParser}
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

