package com.septa97.typeinference

import scala.collection.mutable

object Main {
  // Test here
  def main(args: Array[String]): Unit = {
    val env = mutable.Map[String, Type](
      "+" -> TFun(TNamed("Int"), TFun(TNamed("Int"), TNamed("Int"))),
      "!" -> TFun(TNamed("Bool"), TNamed("Bool")),
      "&&" -> TFun(TNamed("Bool"), TFun(TNamed("Bool"), TNamed("Bool"))),
      "||" -> TFun(TNamed("Bool"), TFun(TNamed("Bool"), TNamed("Bool"))),
      "true" -> TNamed("Bool"),
      "false" -> TNamed("Bool")
    )
    val ctx = Context(next = 0, env = env)
    val expr = ECall(ECall(EVar("+"), EInt(1)), EInt(2))
    // val expr = ECall(ECall(EVar("&&"), EVar("true")), EVar("false"))

    val (_, subst) = Inference.infer(expr, ctx)

    for {
      (key, value) <- subst
    } {
      println(s"$key -> ${Helpers.typeToString(value)}")
    }
  }
}
