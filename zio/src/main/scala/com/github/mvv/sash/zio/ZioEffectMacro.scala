package com.github.mvv.sash.zio

import com.github.mvv.sash.EffectMacro
import zio.{ZIO, ZManaged}

import scala.reflect.macros.blackbox.Context

object ZioEffectMacro {
  def effectImpl[R, E, A](c: Context)(body: c.Expr[ZIO[R, E, A]])(
      implicit envTag: c.WeakTypeTag[R],
      errorTag: c.WeakTypeTag[E],
      resultTag: c.WeakTypeTag[A]): c.Expr[ZIO[R, E, A]] = {
    import c.universe._
    val unit = q"_root_.zio.ZIO.unit"
    val flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    val raise = { value: Tree =>
      q"_root_.zio.ZIO.fail($value)"
    }
    val recover = { (action: Tree, handler: Tree) =>
      q"$action.catchSome($handler)"
    }
    val ensuring = { (action: Tree, finalizer: Tree) =>
      q"$action.ensuring($finalizer)"
    }
    val ensuringType = c.typecheck(unit).tpe
    val bodyType = c.typecheck(q"(null: _root_.zio.ZIO[${envTag.tpe}, ${errorTag.tpe}, ${resultTag.tpe}])").tpe
    EffectMacro.effectImpl(c)(
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

  def managedImpl[R, E, A](c: Context)(body: c.Expr[ZManaged[R, E, A]])(
      implicit envTag: c.WeakTypeTag[R],
      errorTag: c.WeakTypeTag[E],
      resultTag: c.WeakTypeTag[A]): c.Expr[ZManaged[R, E, A]] = {
    import c.universe._
    val unit = q"_root_.zio.ZManaged.fromEffect(_root_.zio.ZIO.unit)"
    val flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    val raise = { value: Tree =>
      q"_root_.zio.ZManaged.fromEffect(_root_.zio.ZIO.fail($value))"
    }
    val bodyType =
      c.typecheck(q"(null: _root_.zio.ZManaged[${envTag.tpe}, ${errorTag.tpe}, ${resultTag.tpe}])").tpe
    EffectMacro.effectImpl(c)(
      predef = Seq.empty,
      unit = Some(unit),
      flatMap = flatMap,
      raise = Some(raise),
      recover = None,
      ensuring = None,
      ensuringType = None,
      body = body,
      bodyType = bodyType
    )
  }
}
