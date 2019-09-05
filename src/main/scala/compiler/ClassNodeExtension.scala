package compiler

import ast.{Clazz, Property}
import jdk.internal.org.objectweb.asm.Opcodes.{ACC_PRIVATE, ACC_PUBLIC, INVOKESPECIAL}
import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._

object Extensions {
  implicit class ClassNodeExtension(cn: ClassNode) {

    def defineGetter(property: Property): Unit = {
      val postFix = s"${property.name.charAt(0).toUpper}${property.name.substring(1, property.name.length)}"
      val getterName = s"get$postFix"
      val getterNode = new MethodNode(ACC_PUBLIC, getterName, "()Ljava/lang/String;", null, Array())
      getterNode.instructions.add(new VarInsnNode(Opcodes.ALOAD, 0))
      getterNode.instructions.add(new FieldInsnNode(Opcodes.GETFIELD, cn.name, property.name, "Ljava/lang/String;"))
      getterNode.instructions.add(new InsnNode(Opcodes.ARETURN))
      cn.methods.add(getterNode)
    }

    def defineSetter(property: Property): Unit = {
      val postFix = s"${property.name.charAt(0).toUpper}${property.name.substring(1, property.name.length)}"
      val setterName = s"set$postFix"
      val setterNode = new MethodNode(ACC_PUBLIC, setterName, "(Ljava/lang/String;)V", null, Array())
      setterNode.instructions.add(new VarInsnNode(Opcodes.ALOAD, 0))
      setterNode.instructions.add(new VarInsnNode(Opcodes.ALOAD, 1))
      setterNode.instructions.add(new FieldInsnNode(Opcodes.PUTFIELD, cn.name, property.name, "Ljava/lang/String;"))
      setterNode.instructions.add(new InsnNode(Opcodes.RETURN))
      cn.methods.add(setterNode)
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
      val methodNode = new MethodNode(ACC_PUBLIC, "<init>", "()V", null, Array())
      methodNode.instructions.add(new VarInsnNode(Opcodes.ALOAD, 0))
      methodNode.instructions.add(new MethodInsnNode(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false))
      clazz.children.foreach {
        case property: Property =>
          methodNode.instructions.add(new VarInsnNode(Opcodes.ALOAD, 0))
          methodNode.instructions.add(new LdcInsnNode("Currently only strings"))
          methodNode.instructions.add(new FieldInsnNode(Opcodes.PUTFIELD, clazz.name, property.name, "Ljava/lang/String;"))
      }
      methodNode.instructions.add(new InsnNode(Opcodes.RETURN))
      cn.methods.add(methodNode)
    }
  }
}
