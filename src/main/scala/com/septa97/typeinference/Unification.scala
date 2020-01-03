package com.septa97.typeinference

import com.septa97.typeinference.Constants.Substitution
import com.septa97.typeinference.Helpers.typeToString

import scala.collection.mutable

object Unification {
  def unify(t1: Type, t2: Type): Substitution = {
    if (t1.nodeType == "Named" && t2.nodeType == "Named" && t1
          .asInstanceOf[TNamed]
          .name == t2.asInstanceOf[TNamed].name) {
      mutable.Map.empty[String, Type]
    } else if (t1.nodeType == "Var") {
      varBind(t1.asInstanceOf[TVar].name, t2)
    } else if (t2.nodeType == "Var") {
      varBind(t2.asInstanceOf[TVar].name, t1)
    } else if (t1.nodeType == "Function" && t2.nodeType == "Function") {
      val tt1 = t1.asInstanceOf[TFun]
      val tt2 = t2.asInstanceOf[TFun]
      val s1 = unify(tt1.from, tt2.from)
      val s2 = unify(tt1.to.applySubst(s1), tt2.to.applySubst(s1))
      composeSubst(s1, s2)
    } else {
      val message =
        s"""
           |Type mismatch:
           |Expected: ${typeToString(t2)}
           |Found: ${typeToString(t1)}
           |""".stripMargin
      throw new IllegalStateException(message)
    }
  }

  def varBind(name: String, ttype: Type): Substitution = {
    if (ttype.nodeType == "Var" && ttype.asInstanceOf[TVar].name == name) {
      mutable.Map.empty[String, Type]
    } else if (contains(ttype, name)) {
      throw new IllegalStateException(
        s"Type ${typeToString(ttype)} contains a reference to itself")
    } else {
      val subst = mutable.Map.empty[String, Type]
      subst += name -> ttype
      subst
    }
  }

  def composeSubst(s1: Substitution, s2: Substitution): Substitution = {
    val result = mutable.Map[String, Type]()

    for {
      (key, ttype) <- s2
    } {
      result(key) = ttype.applySubst(s1)
    }

    s1 ++ result
  }

  def contains(ttype: Type, name: String): Boolean = {
    ttype.nodeType match {
      case "Named" =>
        false
      case "Var" =>
        ttype.asInstanceOf[TVar].name == name
      case "Function" =>
        val t = ttype.asInstanceOf[TFun]
        contains(t.from, name) || contains(t.to, name)
    }
  }
}
