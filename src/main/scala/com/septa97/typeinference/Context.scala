package com.septa97.typeinference

import com.septa97.typeinference.Constants.{Environment, Substitution}

case class Context(var next: Int, env: Environment) {
  def addNewBinding(name: String, ttype: Type): Context = {
    val newCtx = this.copy()
    newCtx.env(name) = ttype
    newCtx
  }

  // For temporary type variables
  def newTVar(): Type = {
    val newVarNum = this.next
    this.next += 1
    TVar(name = s"T$newVarNum")
  }

  def applySubst(subst: Substitution): Context = {
    val newCtx = this.copy()

    for {
      (name, ttype) <- newCtx.env
    } {
      newCtx.env += name -> ttype.applySubst(subst)
    }

    newCtx
  }
}
