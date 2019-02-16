package com.github.mvv.sash.zio.test

import com.github.mvv.sash.zio._
import org.specs2.mutable.Specification
import scalaz.zio.{RTS, Ref}

class ZioEffectSpec extends Specification with RTS {
  "ZIO effect macro should" >> {
    "handle simple programs" >> {
      val result = effect {
        val ref = +Ref.make(1)
        ref.update(_ + 2)
        ref.get
      }
      unsafeRun(result) shouldEqual 3
    }

    "handle try-catch-finally" >> {
      val result = effect[Throwable] {
        val ref = +Ref.make(0)
        try {
          ref.update(_ + 2)
          throw new UnsupportedOperationException
        } catch {
          case _: UnsupportedOperationException =>
            ref.update(_ + 3)
        } finally {
          ref.update(_ + 5)
        }
        ref.get
      }
      unsafeRun(result) shouldEqual 10
    }
  }
}
