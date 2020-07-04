package com.github.mvv.sash.test

sealed trait TM[+A] {
  final def flatMap[B](f: A => TM[B]): TM[B] = TM.FlatMap(this, f)
  final def protect[B >: A](handler: Throwable => TM[B]): TM[B] = TM.Protect(this, handler)
  final def trace: TM.Trace[A] = TM.trace(this)
}

object TM {
  sealed trait Trace[+A]
  object Trace {
    final case class Push[A](trace: Trace[A]) extends Trace[A]
    final case class Pop[A, B](value: A, trace: Trace[B]) extends Trace[B]
    final case class Said[A](word: String, trace: Trace[A]) extends Trace[A]
    final case class Raised[A](error: Throwable, trace: Trace[A]) extends Trace[A]
    final case class Protected[A](trace: Trace[A]) extends Trace[A]
    final case class Unprotected[A](trace: Trace[A]) extends Trace[A]
    final case class Handled[A](trace: Trace[A]) extends Trace[A]
    final case class Failed(error: Throwable) extends Trace[Nothing]
    final case class Done[A](value: A) extends Trace[A]
  }

  final private case class Point[A](value: A) extends TM[A]
  final private case class Raise(error: Throwable) extends TM[Nothing]
  final private case class Say(word: String) extends TM[Unit]
  final private case class Protect[A](tm: TM[A], handler: Throwable => TM[A]) extends TM[A]
  final private case class FlatMap[A, B](tm: TM[A], f: A => TM[B]) extends TM[B]

  final def traceWith[A, B](tm: TM[A], andThen: A => Trace[B], onError: Throwable => Trace[B]): Trace[B] =
    tm match {
      case Point(value) =>
        andThen(value)
      case Raise(error) =>
        Trace.Raised(error, onError(error))
      case Say(word) =>
        Trace.Said(word, andThen(()))
      case Protect(prot, handler) =>
        Trace.Protected {
          traceWith(
            prot,
            (value: A) => Trace.Unprotected(andThen(value)),
            { error: Throwable =>
              Trace.Handled(traceWith(handler(error), andThen, onError))
            }
          )
        }
      case FlatMap(bound, f) =>
        Trace.Push {
          traceWith(bound, (value: Any) => Trace.Pop(value, traceWith(f(value), andThen, onError)), onError)
        }
    }

  def trace[A](tm: TM[A]): Trace[A] = traceWith(tm, Trace.Done[A], Trace.Failed)

  def apply[A](value: A): TM[A] = Point(value)
  def say(word: String): TM[Unit] = Say(word)
  def raise(error: Throwable): TM[Nothing] = Raise(error)
}
