package com.github.mvv.sash.zio

import _root_.zio.stream.ZStream

import scala.language.experimental.macros

package object streams extends ZioStreamEffectful {
  final class stream[R, E] {
    def apply[A](body: ZStream[R, E, A]): ZStream[R, E, A] = macro ZioStreamMacro.streamImpl[R, E, A]
  }
  def stream[R, E]: stream[R, E] = new stream[R, E]
  def streamR[R]: stream[R, Nothing] = new stream[R, Nothing]
  def streamE[E]: stream[Any, E] = new stream[Any, E]
}
