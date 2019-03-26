package com.github.mvv.sash.zio

import com.github.mvv.sash.{effectful, outsideOfMacro, Effectful}
import scalaz.zio.{ZIO, ZManaged}
import scalaz.zio.stream.ZStream

// IDEA fails to resolve 'Type3EffecfulOp', see https://youtrack.jetbrains.com/issue/SCL-14989
trait ZioEffectful extends Effectful {
  implicit final class ZioEffectfulOp[R, E, A](io: ZIO[R, E, A]) {
    @effectful
    def unary_+(): A = outsideOfMacro
  }
  implicit final class ZStreamEffectfulOp[R, E, A](stream: ZStream[R, E, A]) {
    @effectful
    def unary_+(): A = outsideOfMacro
  }
  implicit final class ZManagedEffectfulOp[R, E, A](managed: ZManaged[R, E, A]) {
    @effectful
    def unary_+(): A = outsideOfMacro
  }
}
