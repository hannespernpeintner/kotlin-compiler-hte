package compiler

import java.util

import ast.{Clazz, IntExpression, Property, Val}
import org.scalatest.FlatSpec

class BytecodeCompilerSpec extends FlatSpec {

  "An empty class" should "be compiled correctly" in {
    val file = ast.File()
    file.children += Clazz("MyClass", Option(file))

    val classFiles = new BytecodeCompiler().compile(file)
    assert(classFiles.size == 1)
    val myClassClassFile = classFiles.head
    assert(myClassClassFile.fileName == "MyClass.class")

    val loadedClazz = new ByteArrayClassLoader(myClassClassFile.content).loadClass("MyClass")
    assert(loadedClazz.getSimpleName == "MyClass")
    assert(loadedClazz.getDeclaredFields.isEmpty)
  }

  "A class with a property" should "be compiled correctly" in {
    val file = ast.File()
    val clazz = Clazz("MyClass", Option(file))
    file.children += clazz
    clazz.children += Property(Val, "myProperty", IntExpression(1, None), Option(clazz))

    val classFiles = new BytecodeCompiler().compile(file)
    assert(classFiles.size == 1)
    val myClassClassFile = classFiles.head
    assert(myClassClassFile.fileName == "MyClass.class")

    val loadedClazz = new ByteArrayClassLoader(myClassClassFile.content).loadClass("MyClass")
    assert(loadedClazz.getSimpleName == "MyClass")
    assert(loadedClazz.getDeclaredFields.length == 1)
    assert(loadedClazz.getDeclaredFields.toList.head.getName == "myProperty")
    val instance = loadedClazz.newInstance()
    val propertyGetter = loadedClazz.getDeclaredMethods.filter(_.getName == "getMyProperty").head
    val propertyValue = propertyGetter.invoke(instance)
    assert(propertyValue == "Currently only strings")
    loadedClazz.getDeclaredMethods.filter(_.getName == "setMyProperty").head.invoke(instance, "xxxxx")
    assert(propertyGetter.invoke(instance) == "xxxxx")
  }
}

class ByteArrayClassLoader(val bytes: Array[Byte]) extends ClassLoader {

  override def findClass(name: String): Class[_] = {
    defineClass(name,bytes,0,bytes.length);
  }

}