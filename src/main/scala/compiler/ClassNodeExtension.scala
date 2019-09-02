package compiler

import ast.{Clazz, Property}
import jdk.internal.org.objectweb.asm.Opcodes.{ACC_PRIVATE, ACC_PUBLIC, INVOKESPECIAL}
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{ClassNode, FieldNode, MethodNode}

object Extensions {
  implicit class ClassNodeExtension(cn: ClassNode) {

    def defineGetter(property: Property): Unit = {
      val postFix = s"${property.name.charAt(0).toUpper}${property.name.substring(1, property.name.length)}"
      val getterName = s"get$postFix"
      val getterNode = new MethodNode(ACC_PUBLIC, getterName, "()Ljava/lang/String;", null, Array())
      cn.methods.add(getterNode)
      getterNode.visitCode()
      getterNode.visitIntInsn(Opcodes.ALOAD, 0)
      getterNode.visitFieldInsn(Opcodes.GETFIELD, cn.name, property.name, "Ljava/lang/String;")
      getterNode.visitInsn(Opcodes.ARETURN)
      getterNode.visitEnd()
    }

    def defineSetter(property: Property): Unit = {
      val postFix = s"${property.name.charAt(0).toUpper}${property.name.substring(1, property.name.length)}"
      val setterName = s"set$postFix"
      val setterNode = new MethodNode(ACC_PUBLIC, setterName, "(Ljava/lang/String;)V", null, Array())
      cn.methods.add(setterNode)
      setterNode.visitCode()
      setterNode.visitIntInsn(Opcodes.ALOAD, 0)
      setterNode.visitIntInsn(Opcodes.ALOAD, 1)
      setterNode.visitFieldInsn(Opcodes.PUTFIELD, cn.name, property.name, "Ljava/lang/String;")
      setterNode.visitInsn(Opcodes.RETURN)
      setterNode.visitEnd()
    }
    def defineField(property: Property): Unit = {
      cn.fields.add(new FieldNode(ACC_PRIVATE,property.name, "Ljava/lang/String;", null, null))
    }

    def defineProperty(property: Property): Unit = {
      defineField(property)
      defineGetter(property)
      defineSetter(property)
    }

    def defineNoArgsConstructor(clazz: Clazz): Unit = {
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
    }
  }
}
