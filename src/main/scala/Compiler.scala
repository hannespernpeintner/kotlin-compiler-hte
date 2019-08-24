import kastree.ast.Node



trait Compiler {
  def compile(file: Node.File): List[String]
}

class JvmByteCodeCompiler extends Compiler {
  override def compile(file: Node.File): List[String] = {
    return List.empty[String]
  }
}
