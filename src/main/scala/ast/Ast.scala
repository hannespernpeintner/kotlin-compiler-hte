package ast

class Ast {

}

sealed trait Instruction {
  sealed trait Statement {

  }
  sealed trait Expression {
    sealed trait Declaration extends Expression {
      class Function extends Declaration
      class Class extends Declaration
    }
  }
}

