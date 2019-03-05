package com.github.mvv.sash.simple

import com.github.mvv.sash.EffectMacro

import scala.reflect.macros.blackbox.Context

object SimpleEffectMacro extends {
  final def effectImpl[A](ctx: Context)(body: ctx.Expr[A])(implicit bodyTag: ctx.WeakTypeTag[A]): ctx.Expr[A] = {
    import ctx.universe._
    def flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    val impl = new EffectMacro { override val c: ctx.type = ctx }
    impl.effectImpl(predef = Seq.empty,
                    unit = None,
                    flatMap = flatMap,
                    raise = None,
                    recover = None,
                    ensuring = None,
                    ensuringType = None,
                    body = body,
                    bodyType = bodyTag.tpe)
  }
}
