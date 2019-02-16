package com.github.mvv.sash.zio

import com.github.mvv.sash.{effectful, outsideOfMacro, Effectful}
import scalaz.zio.IO
import scalaz.zio.stream.Stream

// IDEA fails to resolve 'Type2EffecfulOp', see https://youtrack.jetbrains.com/issue/SCL-14989
trait ZioEffectful extends Effectful {
  implicit final class ZioIoEffectfulOp[E, A](io: IO[E, A]) {
    @effectful
    def unary_+(): A = outsideOfMacro
  }
  implicit final class ZioStreamEffectfulOp[E, A](stream: Stream[E, A]) {
    @effectful
    def unary_+(): A = outsideOfMacro
  }
}
