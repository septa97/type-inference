package com.septa97.typeinference

import com.septa97.typeinference.Constants.Substitution

sealed trait Type {
  def nodeType: String

  def applySubst(subst: Substitution): Type = {
    this match {
      case _: TNamed =>
        this
      case t: TVar =>
        subst.get(t.name) match {
          case Some(t) => t
          case None    => this
        }
      case func: TFun =>
        TFun(from = func.from.applySubst(subst), to = func.to.applySubst(subst))
    }
  }
}
case class TNamed(name: String) extends Type {
  override def nodeType: String = "Named"
}
case class TVar(name: String) extends Type {
  override def nodeType: String = "Var"
}
case class TFun(from: Type, to: Type) extends Type {
  override def nodeType: String = "Function"
}
