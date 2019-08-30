package compiler

import java.util

import ast.Clazz
import org.scalatest.FlatSpec

class BytecodeCompilerSpec extends FlatSpec {

  "A empty class" should "be compiled correctly" in {
    val file = ast.File()
    file.children += Clazz("MyClass", Option(file))

    val classFiles = new BytecodeCompiler().compile(file)
    assert(classFiles.size == 1)
    val myClassClassFile = classFiles.head
    assert(myClassClassFile.fileName == "MyClass.class")

    val loadedClazz = new ByteArrayClassLoader(myClassClassFile.content).loadClass("MyClass")
    assert(loadedClazz.getSimpleName == "MyClass")
  }

}

class ByteArrayClassLoader(val bytes: Array[Byte]) extends ClassLoader {

  override def findClass(name: String) = {
    defineClass(name,bytes,0,bytes.length);
  }

}