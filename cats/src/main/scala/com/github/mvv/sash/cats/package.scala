package com.github.mvv.sash

import scala.language.higherKinds
import scala.language.experimental.macros

package object cats extends Effectful with Impurity with Purity {
  final class effect[F[_]] {
    def apply[A](body: F[A]): F[A] = macro CatsEffectMacro.effectImpl[F, A]
  }
  def effect[F[_]]: effect[F] = new effect[F]
}
