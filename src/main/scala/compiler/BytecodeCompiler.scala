package compiler

import ast.{Ast, Clazz, File}
import jdk.internal.org.objectweb.asm.Opcodes
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.tree.ClassNode

case class ClassFile(val fileName: String, val content: Array[Byte])

class BytecodeCompiler {
  def compile(file: File): List[ClassFile] = file.children.map {
    case clazz: Clazz =>
      val content = "Compiled from \"unknown\"\n" +
        s"public class ${clazz.name} {" +
        ""
      val cn = new ClassNode()
      cn.version = Opcodes.V1_8
      cn.access = Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT + Opcodes.ACC_INTERFACE
      cn.superName = "java/lang/Object"
      cn.name = clazz.name

      val writer = new ClassWriter(0)
      cn.accept(writer)

      ClassFile(s"${clazz.name}.class", writer.toByteArray)
  }.toList
}
