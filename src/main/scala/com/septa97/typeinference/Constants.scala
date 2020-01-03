package com.septa97.typeinference

import scala.collection.mutable

object Constants {
  type Substitution = mutable.Map[String, Type]
  type Environment = mutable.Map[String, Type]
}
