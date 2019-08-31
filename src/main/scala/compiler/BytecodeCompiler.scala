package compiler

import ast.{Clazz, File, Property}
import jdk.internal.org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Opcodes, Type}
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

      val constructor = cn.visitMethod(ACC_PUBLIC, "<init>", "()V", null, Array())
      constructor.visitCode()
      constructor.visitIntInsn(Opcodes.ALOAD, 0)
      constructor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
      clazz.children.foreach {
        case property: Property =>
          constructor.visitIntInsn(Opcodes.ALOAD, 0)
          constructor.visitLdcInsn("Currently only strings")
          constructor.visitFieldInsn(Opcodes.PUTFIELD, clazz.name, property.name, "Ljava/lang/String;")
      }
      constructor.visitInsn(Opcodes.RETURN)
      constructor.visitEnd()

      clazz.children.foreach {
        case property: Property => {
          cn.visitField(ACC_PRIVATE, property.name, "Ljava/lang/String;", null, "Currently only strings").visitEnd()
          val postFix = s"${property.name.charAt(0).toUpper}${property.name.substring(1, property.name.length)}"
          val getterName = s"get$postFix"
          val getterMethodVisitor = cn.visitMethod(ACC_PUBLIC, getterName, "()Ljava/lang/String;", null, Array())
          getterMethodVisitor.visitCode()
          getterMethodVisitor.visitIntInsn(Opcodes.ALOAD, 0)
          getterMethodVisitor.visitFieldInsn(Opcodes.GETFIELD, clazz.name, property.name, "Ljava/lang/String;")
          getterMethodVisitor.visitInsn(Opcodes.ARETURN)
          getterMethodVisitor.visitEnd()

          val setterName = s"set$postFix"
          val setterMethodVisitor = cn.visitMethod(ACC_PUBLIC, setterName, "(Ljava/lang/String;)V", null, Array())
          setterMethodVisitor.visitCode()
          setterMethodVisitor.visitIntInsn(Opcodes.ALOAD, 0)
          setterMethodVisitor.visitIntInsn(Opcodes.ALOAD, 1)
          setterMethodVisitor.visitFieldInsn(Opcodes.PUTFIELD, clazz.name, property.name, "Ljava/lang/String;")
          setterMethodVisitor.visitInsn(Opcodes.RETURN)
          setterMethodVisitor.visitEnd()
        }
      }
      cn.visitEnd()

      val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
      cn.accept(writer)

      ClassFile(s"${clazz.name}.class", writer.toByteArray)
  }.toList
}
