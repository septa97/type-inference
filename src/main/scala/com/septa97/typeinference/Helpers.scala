package com.septa97.typeinference

object Helpers {
  def typeToString(ttype: Type): String = {
    ttype match {
      case t: TFun   => s"${typeToString(t.from)} -> ${typeToString(t.to)}"
      case t: TVar   => t.name
      case t: TNamed => t.name
    }
  }
}
