package compiler

import ast.{Clazz, File, Property}
import jdk.internal.org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Opcodes, Type}
import org.objectweb.asm.tree.ClassNode
import Extensions.ClassNodeExtension

case class ClassFile(val fileName: String, val content: Array[Byte])

class BytecodeCompiler {
  def compile(file: File): List[ClassFile] = file.children.map {
    case clazz: Clazz =>
      val cn = new ClassNode()
      cn.version = V1_8
      cn.access = ACC_PUBLIC
      cn.superName = "java/lang/Object"
      cn.name = clazz.name

      cn.defineNoArgsConstructor(clazz)

      clazz.children.foreach {
        case property: Property => cn.defineProperty(property)
      }

      val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
      cn.accept(writer)

      ClassFile(s"${clazz.name}.class", writer.toByteArray)
  }.toList
}
