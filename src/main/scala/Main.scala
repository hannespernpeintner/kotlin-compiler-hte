import de.hanno.kotlin.{KotlinParser, KotlinParserBaseListener}
import lexerparser.{KotlinFileTreeWalker, LexerParser}

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

