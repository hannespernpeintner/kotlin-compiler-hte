package ast

import org.scalatest.FlatSpec

class ClassLevelAstSpec extends FlatSpec {

  def commonAssertions(ast: List[Ast]): Unit = {
    assert(ast.size == 1)
    val classDef = ast.head
    assert(classDef.isInstanceOf[Clazz])
    assert(classDef.asInstanceOf[Clazz].name == "Foo")
    assert(classDef.children.size == 1)
  }

  "A class level expression body function" should "be parsed correctly" in {
    val code = "class Foo { fun bar() = println(1) }"
    val ast = new AstCreator().create(code)
    commonAssertions(ast)
    assert(ast.head.children.head.asInstanceOf[Function].bodyType == ExpressionBody)
  }

  "A regular class level function" should "be parsed correctly" in {
    val code = "class Foo { fun bar() { println(1) } }"
    val ast = new AstCreator().create(code)
    commonAssertions(ast)
    assert(ast.head.children.head.asInstanceOf[Function].bodyType == RegularFunctionBody)
  }


  "A complex class level function" should "be parsed correctly" in {
    val code = """
              |class Foo {
              |    fun bar() {
              |         fun foo() = println(1)
              |         foo()
              |    }
              |}""".stripMargin

    val ast = new AstCreator().create(code)
    commonAssertions(ast)
    val barFunction = ast.head.children.head.asInstanceOf[Function]
    assert(barFunction.bodyType == RegularFunctionBody)
    assert(barFunction.children.size == 2)
    val fooFunction = barFunction.children.get(0).get.asInstanceOf[Function]
    assert(fooFunction.name == "foo")
    assert(fooFunction.children.size == 1)
    val printlnFunctionCall = fooFunction.children.get(0).get.asInstanceOf[FunctionCall]
    assert(printlnFunctionCall.name == "println")
    assert(printlnFunctionCall.receiver.isEmpty)
    assert(printlnFunctionCall.params.get == "1")
    val fooFunctionCall = barFunction.children.get(1).get.asInstanceOf[FunctionCall]
    assert(fooFunctionCall.name == "foo")
  }

}
