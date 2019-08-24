package ast

import org.scalatest.FlatSpec

class AstSpec extends FlatSpec {

  "A top level function" should "be parsed correctly" in {
    val code = "fun bar() = println(1)"
    val ast = new AstCreator().create(code)
    assert(ast.size == 1)
    assert(ast.head.isInstanceOf[Function])
  }

}
