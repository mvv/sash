package com.github.mvv.sash

import scala.language.experimental.macros

package object simple extends Effectful with Impurity with Purity {
  def effect[A](body: A): A = macro SimpleEffectMacro.effectImpl[A]
}
