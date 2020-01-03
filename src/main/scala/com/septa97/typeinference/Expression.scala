package com.septa97.typeinference

sealed trait Expression {
  def nodeType: String
}

case class EInt(value: Int) extends Expression {
  override def nodeType: String = "Int"
}
case class EVar(name: String) extends Expression {
  override def nodeType: String = "Var"
}
case class EFunc(param: String, body: Expression) extends Expression {
  override def nodeType: String = "Function"
}
case class ECall(func: Expression, arg: Expression) extends Expression {
  override def nodeType: String = "Call"
}
case class EIf(cond: Expression,
               trueBranch: Expression,
               falseBranch: Expression)
    extends Expression {
  override def nodeType: String = "If"
}
