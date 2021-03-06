package com.github.mvv.sash.zio.test

import com.github.mvv.sash.zio._
import org.specs2.mutable.Specification
import zio.{BootstrapRuntime, IO, Managed, Ref}

class ZioEffectSpec extends Specification with BootstrapRuntime {
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
      val result = effectE[Throwable] {
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
        ref.get.map(_.toString)
      }
      unsafeRun(result) shouldEqual "10"
    }
  }

  "ZIO managed macro should" >> {
    "construct managed values" >> {
      val result = effect {
        val ref1 = +Ref.make(1)
        val ref2 = +Ref.make(1)
        val (before1, before2) = +managed {
          Managed.make(ref1.set(2))(_ => ref1.set(3))
          Managed.make(ref2.set(2))(_ => ref2.set(3))
        }.use { _ =>
          effect {
            IO.succeed((+ref1.get, +ref2.get))
          }
        }
        IO.succeed(((before1, before2), (+ref1.get, +ref2.get)))
      }
      unsafeRun(result) shouldEqual ((2, 2), (3, 3))
    }
  }
}
