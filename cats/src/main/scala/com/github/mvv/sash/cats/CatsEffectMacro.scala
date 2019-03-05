package com.github.mvv.sash.cats

import com.github.mvv.sash.EffectMacro

import scala.reflect.macros.blackbox.Context
import scala.language.higherKinds

object CatsEffectMacro {
  def effectImpl[F[_], A](ctx: Context)(body: ctx.Expr[F[A]])(
      implicit monadTag: ctx.WeakTypeTag[F[_]]): ctx.Expr[F[A]] = {
    import ctx.universe._
    val cons = monadTag.tpe.typeConstructor
    val monadInst = TermName(ctx.freshName("monad"))
    val predef = Seq(q"val $monadInst: _root_.cats.Monad[$cons] = implicitly[_root_.cats.Monad[$cons]]")
    val unit = q"$monadInst.unit"
    val flatMap = { value: Tree =>
      q"$monadInst.flatMap($value)"
    }
    val raise = { value: Tree =>
      q"implicitly[_root_.cats.ApplicativeError[$cons, _root_.java.lang.Throwable]].raiseError($value)"
    }
    val recover = { (action: Tree, handler: Tree) =>
      q"implicitly[_root_.cats.ApplicativeError[$cons, _root_.java.lang.Throwable]].recoverWith($action)($handler)"
    }
    val impl = new { val c: ctx.type = ctx } with EffectMacro
    impl.effectImpl(
      predef = predef,
      unit = Some(unit),
      flatMap = flatMap,
      raise = Some(raise),
      recover = Some(recover),
      ensuring = None,
      ensuringType = None,
      body = body,
      bodyType = body.actualType
    )
  }
}
