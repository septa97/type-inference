package com.septa97.typeinference

import com.septa97.typeinference.Constants.Substitution

import scala.collection.mutable

object Inference {
  def infer(expr: Expression, ctx: Context): (Type, Substitution) = {
    expr match {
      case _: EInt =>
        TNamed(name = "Int") -> mutable.Map.empty[String, Type]
      case e: EVar =>
        ctx.env.get(e.name) match {
          case Some(t) => t -> mutable.Map.empty[String, Type]
          case None    => throw new IllegalStateException(s"Unbound var ${e.name}")
        }
      case e: EFunc =>
        val newType = ctx.newTVar()
        val newCtx = ctx.addNewBinding(e.param, newType)
        val (bodyType, subst) = infer(e.body, newCtx)
        val inferredType = TFun(from = newType.applySubst(subst), to = bodyType)
        inferredType -> subst
      case e: ECall =>
        val (funcType, s1) = infer(e.func, ctx)
        val (argType, s2) = infer(e.arg, ctx.applySubst(s1))
        val newVar = ctx.newTVar()
        val s3 = Unification.composeSubst(s1, s2)
        val s4 = Unification.unify(TFun(from = argType, to = newVar), funcType)
        val funcType1 = funcType.applySubst(s4)
        val s5 = Unification.composeSubst(s3, s4)
        val s6 = Unification.unify(
          funcType1.asInstanceOf[TFun].from.applySubst(s5),
          argType)
        val finalSubst = Unification.composeSubst(s5, s6)
        val finalType = funcType1.asInstanceOf[TFun].to.applySubst(finalSubst)
        finalType -> finalSubst
      case e: EIf =>
        val (condType, s0) = infer(e.cond, ctx)
        val s1 = Unification.unify(condType, TNamed(name = "Bool"))
        val ctx1 = ctx.applySubst(Unification.composeSubst(s0, s1))
        val (trueBranchType0, s2) = infer(e.trueBranch, ctx1)
        val ctx2 = ctx1.applySubst(s2)
        val s3 = Unification.composeSubst(s1, s2)
        val (falseBranchType0, s4) = infer(e.falseBranch, ctx2)
        val s5 = Unification.composeSubst(s3, s4)

        val trueBranchType1 = trueBranchType0.applySubst(s5)
        val falseBranchType1 = falseBranchType0.applySubst(s5)
        val s6 = Unification.unify(trueBranchType1, falseBranchType1)
        val finalSubst = Unification.composeSubst(s5, s6)
        val finalType = trueBranchType1.applySubst(finalSubst)

        // either trueBranchType1 or falseBranchType1 will do because they are unified
        finalType -> finalSubst
      case _ =>
        throw new IllegalStateException("Unimplemented type.")
    }
  }
}
