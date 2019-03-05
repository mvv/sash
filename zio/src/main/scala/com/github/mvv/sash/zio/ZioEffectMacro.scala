package com.github.mvv.sash.zio

import com.github.mvv.sash.EffectMacro
import scalaz.zio.IO
import scalaz.zio.stream.Stream

import scala.reflect.macros.blackbox.Context

object ZioEffectMacro {
  def effectImpl[E, A](ctx: Context)(body: ctx.Expr[IO[E, A]])(implicit errorTag: ctx.WeakTypeTag[E],
                                                               resultTag: ctx.WeakTypeTag[A]): ctx.Expr[IO[E, A]] = {
    import ctx.universe._
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
    val ensuringType = ctx.typecheck(unit).tpe
    val bodyType = ctx.typecheck(q"(null: _root_.scalaz.zio.IO[${errorTag.tpe}, ${resultTag.tpe}])").tpe
    val impl = new EffectMacro { override val c: ctx.type = ctx }
    impl.effectImpl(
      predef = Seq.empty,
      unit = Some(unit),
      flatMap = flatMap,
      raise = Some(raise),
      recover = Some(recover),
      ensuring = Some(ensuring),
      ensuringType = Some(ensuringType),
      body = body,
      bodyType = bodyType
    )
  }

  def streamImpl[E, A](ctx: Context)(body: ctx.Expr[Stream[E, A]])(
      implicit errorTag: ctx.WeakTypeTag[E],
      resultTag: ctx.WeakTypeTag[A]): ctx.Expr[Stream[E, A]] = {
    import ctx.universe._
    val unit = q"_root_.scalaz.zio.stream.Stream.succeed(())"
    val flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    val raise = { value: Tree =>
      q"_root_.scalaz.zio.stream.Stream.fail($value)"
    }
    val bodyType = ctx.typecheck(q"(null: _root_.scalaz.zio.stream.Stream[${errorTag.tpe}, ${resultTag.tpe}])").tpe
    val impl = new EffectMacro { override val c: ctx.type = ctx }
    impl.effectImpl(predef = Seq.empty,
                    unit = Some(unit),
                    flatMap = flatMap,
                    raise = Some(raise),
                    recover = None,
                    ensuring = None,
                    ensuringType = None,
                    body = body,
                    bodyType = bodyType)
  }
}
