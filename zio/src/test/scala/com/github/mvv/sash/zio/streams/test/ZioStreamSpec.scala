package com.github.mvv.sash.zio.streams.test

import com.github.mvv.sash.zio._
import com.github.mvv.sash.zio.streams._
import org.specs2.mutable.Specification
import zio.{DefaultRuntime, Ref, ZIO}
import zio.stream.ZStream

class ZioStreamSpec extends Specification with DefaultRuntime {
  "ZIO stream macro should" >> {
    "construct streams" >> {
      val values = stream {
        val ref = +ZStream.fromEffect(Ref.make(0))
        val elem = +ZStream(1, 2, 3, 4, 5)
        ZStream.fromEffect {
          effect {
            ref.update(_ + elem)
            ZIO.succeed(10 - +ref.get)
          }
        }
      }
      unsafeRun(values.runCollect) must containTheSameElementsAs(Seq(9, 7, 4, 0, -5))
    }
  }
}
