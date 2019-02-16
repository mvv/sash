package com.github.mvv.sash

import scalaz.zio.IO
import scalaz.zio.stream.Stream

import scala.language.experimental.macros

package object zio extends ZioEffectful with Impurity with Purity {
  final class effect[E] {
    def apply[A](body: IO[E, A]): IO[E, A] = macro ZioEffectMacro.effectImpl[E, A]
  }
  def effect[E]: effect[E] = new effect[E]

  final class stream[E] {
    def apply[A](body: Stream[E, A]): Stream[E, A] = macro ZioEffectMacro.streamImpl[E, A]
  }
  def stream[E]: stream[E] = new stream[E]
}
