package lexerparser

import de.hanno.kotlin.{KotlinLexer, KotlinParser, KotlinParserBaseListener}
import de.hanno.kotlin.KotlinParser.KotlinFileContext
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

class KotlinFileTreeWalker(file: KotlinFileContext) {
  def walk(listener: KotlinParserBaseListener): Unit = {
    new ParseTreeWalker().walk(listener, file)
  }
}

class LexerParser {
  def read(sourceCode: String): KotlinFileContext = {

    val lexer = new KotlinLexer(CharStreams.fromString(sourceCode))
    val tokens = new CommonTokenStream(lexer)
    val parser = new KotlinParser(tokens)

    parser.kotlinFile()
  }
}