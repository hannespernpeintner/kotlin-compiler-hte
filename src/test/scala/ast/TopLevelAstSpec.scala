package ast

import org.scalatest.FlatSpec

class TopLevelAstSpec extends FlatSpec {

  "A top level function" should "be parsed correctly" in {
    val code = "fun bar() = println(1)"
    val ast = new AstCreator().create(code)
    assert(ast.size == 1)
    assert(ast.head.isInstanceOf[Function])
    val barFunction = ast.head.asInstanceOf[Function]
    assert(barFunction.name == "bar")
    assert(barFunction.bodyType == ExpressionBody)
  }

  "A top level class" should "be parsed correctly" in {
    val code = "class Foo"
    val ast = new AstCreator().create(code)
    assert(ast.size == 1)
    assert(ast.head.isInstanceOf[Clazz])
    assert(ast.head.asInstanceOf[Clazz].name == "Foo")
  }

  "A top level property" should "be parsed correctly" in {
    val code = "val foo: Int = 5"
    val ast = new AstCreator().create(code)
    assert(ast.size == 1)
    assert(ast.head.isInstanceOf[Property])
    assert(ast.head.asInstanceOf[Property].name == "foo")
  }

  "Multiple top level properties" should "be parsed correctly" in {
    val code = "val foo: Int = 5; val foo1: Int = 2; val foo2: Int = 3"
    val ast = new AstCreator().create(code)
    assert(ast.size == 3)
    assert(ast.head.isInstanceOf[Property])
    assert(ast.head.asInstanceOf[Property].name == "foo")
  }

  "A class level expression body function" should "be parsed correctly" in {
    val code =
      """class Foo {
        |   fun bar() = 2
        |}
        |val foo = Foo()
        |val bar = foo.bar()""".stripMargin
    val ast = new AstCreator().create(code)
    assert(ast.size == 3)
    val classDef = ast.head
    assert(classDef.isInstanceOf[Clazz])
    assert(classDef.asInstanceOf[Clazz].name == "Foo")
    assert(classDef.children.size == 1)
    assert(ast.head.children.head.asInstanceOf[Function].bodyType == ExpressionBody)

    val foo = ast(1)
    assert(foo.isInstanceOf[Property])
    assert(foo.asInstanceOf[Property].propertyType == Val)
    assert(foo.asInstanceOf[Property].name == "foo")

    val barCall = ast(2)
    assert(barCall.isInstanceOf[Property])
    assert(barCall.asInstanceOf[Property].name == "bar")
    val barFunctionCall = barCall.asInstanceOf[Property].value
    assert(barFunctionCall.isInstanceOf[FunctionCall])
    assert(barFunctionCall.asInstanceOf[FunctionCall].name == "bar")
    assert(barFunctionCall.asInstanceOf[FunctionCall].params.isEmpty)
  }
}
