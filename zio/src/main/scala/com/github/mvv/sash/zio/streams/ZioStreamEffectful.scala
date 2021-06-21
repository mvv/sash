package com.github.mvv.sash.zio.streams

import com.github.mvv.sash.{effectful, outsideOfMacro, Effectful}
import zio.stream.ZStream

// IDEA fails to resolve 'Type3EffecfulOp', see https://youtrack.jetbrains.com/issue/SCL-14989
trait ZioStreamEffectful extends Effectful {
  implicit final class ZStreamEffectfulOp[R, E, A](stream: ZStream[R, E, A]) {
    @effectful
    def unary_+ : A = outsideOfMacro
  }
}
