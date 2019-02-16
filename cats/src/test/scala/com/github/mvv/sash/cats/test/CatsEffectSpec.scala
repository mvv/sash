package com.github.mvv.sash.cats.test

import cats.MonadError
import com.github.mvv.sash.cats._
import com.github.mvv.sash.test.TM
import org.specs2.mutable.Specification

class CatsEffectSpec extends Specification {
  import TM.Trace._

  implicit val traceMonad: MonadError[TM, Throwable] = new MonadError[TM, Throwable] {
    override def pure[A](value: A): TM[A] = TM(value)
    override def flatMap[A, B](m: TM[A])(f: A => TM[B]): TM[B] = m.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => TM[Either[A, B]]): TM[B] = f(a).flatMap {
      case Left(next) => tailRecM(next)(f)
      case Right(b)   => TM(b)
    }
    override def raiseError[A](error: Throwable): TM[A] = TM.raise(error)
    override def handleErrorWith[A](m: TM[A])(f: Throwable => TM[A]): TM[A] = m.protect(f)
  }

  "Cats effect macro should" >> {
    "handle throws" >> {
      val error = new UnsupportedOperationException
      effect[TM] {
        throw error
        TM(())
      }.trace shouldEqual Push(Raised(error, Failed(error)))
    }

    "handle catches" >> {
      effect[TM] {
        try {
          TM.say("trying")
          TM.say("hard")
        } catch {
          case _: Throwable =>
            TM.say("handled")
        }
        TM.say("end")
      }.trace shouldEqual Push(Protected(Push(Said("trying", Pop((), Said("hard",
                            Unprotected(Pop((), Said("end", Done(()))))))))))
    }

    "handle hitting catches" >> {
      val error = new UnsupportedOperationException
      effect[TM] {
        try throw error
        catch {
          case _: UnsupportedOperationException =>
            TM.say("handled")
            TM(1)
        }
      }.trace shouldEqual Protected(Raised(error, Handled(Push(Said("handled", Pop((), Done(1)))))))
    }

    "handle missing catches" >> {
      val error = new UnsupportedOperationException
      effect[TM] {
        try throw error
        catch {
          case _: IllegalArgumentException =>
            TM.say("handled")
        }
        TM.say("unreachable")
      }.trace shouldEqual Push(Protected(Raised(error, Handled(Raised(error, Failed(error))))))
    }
  }
}
