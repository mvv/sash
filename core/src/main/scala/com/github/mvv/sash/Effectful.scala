package com.github.mvv.sash

import scala.language.higherKinds

trait Effectful {
  implicit final class Type1EffectfulOp[Z[_], A](value: Z[A]) {
    @effectful
    def unary_+(): A = outsideOfMacro
  }
  implicit final class Type2EffectfulOp[Z[_, _], A, B](value: Z[A, B]) {
    @effectful
    def unary_+(): B = outsideOfMacro
  }
  implicit final class Type3EffectfulOp[Z[_, _, _], A, B, C](value: Z[A, B, C]) {
    @effectful
    def unary_+(): C = outsideOfMacro
  }
  implicit final class Type4EffectfulOp[Z[_, _, _, _], A, B, C, D](value: Z[A, B, C, D]) {
    @effectful
    def unary_+(): D = outsideOfMacro
  }
}
