package com.github.mvv.sash.zio

import com.github.mvv.sash.EffectMacro
import scalaz.zio.IO
import scalaz.zio.stream.Stream

import scala.reflect.macros.blackbox.Context

object ZioEffectMacro {
  def effectImpl[E, A](c: Context)(body: c.Expr[IO[E, A]]): c.Expr[IO[E, A]] = {
    import c.universe._
    val unit = q"_root_.scalaz.zio.IO.unit"
    val flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    val raise = { value: Tree =>
      q"_root_.scalaz.zio.IO.fail($value)"
    }
    val recover = { (action: Tree, handler: Tree) =>
      q"$action.catchSome($handler)"
    }
    val ensuring = { (action: Tree, finalizer: Tree) =>
      q"$action.ensuring($finalizer)"
    }
    EffectMacro.effectImpl(c)(predef = Seq.empty,
                              unit = Some(unit),
                              flatMap = flatMap,
                              raise = Some(raise),
                              recover = Some(recover),
                              ensuring = Some(ensuring),
                              body = body)
  }

  def streamImpl[E, A](c: Context)(body: c.Expr[Stream[E, A]]): c.Expr[Stream[E, A]] = {
    import c.universe._
    val unit = q"_root_.scalaz.zio.stream.Stream.succeed(())"
    val flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    val raise = { value: Tree =>
      q"_root_.scalaz.zio.stream.Stream.fail($value)"
    }
    EffectMacro.effectImpl(c)(predef = Seq.empty,
                              unit = Some(unit),
                              flatMap = flatMap,
                              raise = Some(raise),
                              recover = None,
                              ensuring = None,
                              body = body)
  }
}
