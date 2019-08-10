package com.github.mvv.sash

import _root_.zio.{ZIO, ZManaged}

import scala.language.experimental.macros

package object zio extends ZioEffectful with Impurity with Purity {
  final class effect[R, E] {
    def apply[A](body: ZIO[R, E, A]): ZIO[R, E, A] = macro ZioEffectMacro.effectImpl[R, E, A]
  }
  def effect[R, E]: effect[R, E] = new effect[R, E]
  def effectR[R]: effect[R, Nothing] = new effect[R, Nothing]
  def effectE[E]: effect[Any, E] = new effect[Any, E]

  final class managed[R, E] {
    def apply[A](body: ZManaged[R, E, A]): ZManaged[R, E, A] = macro ZioEffectMacro.managedImpl[R, E, A]
  }
  def managed[R, E]: managed[R, E] = new managed[R, E]
  def managedR[R]: managed[R, Nothing] = new managed[R, Nothing]
  def managedE[E]: managed[Any, E] = new managed[Any, E]
}
