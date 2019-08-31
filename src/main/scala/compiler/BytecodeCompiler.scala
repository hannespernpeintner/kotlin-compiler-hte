package compiler

import ast.{Clazz, File, Property}
import jdk.internal.org.objectweb.asm.Opcodes._
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.tree.ClassNode

case class ClassFile(val fileName: String, val content: Array[Byte])

class BytecodeCompiler {
  def compile(file: File): List[ClassFile] = file.children.map {
    case clazz: Clazz =>
      val cn = new ClassNode()
      cn.version = V1_8
      cn.access = ACC_PUBLIC
      cn.superName = "java/lang/Object"
      cn.name = clazz.name
      clazz.children.foreach {
        case property: Property => {
          cn.visitField(ACC_PRIVATE, property.name, "Ljava/lang/String;", null, "Currently only strings").visitEnd()
        }
      }

      val writer = new ClassWriter(0)
      cn.accept(writer)

      ClassFile(s"${clazz.name}.class", writer.toByteArray)
  }.toList
}
