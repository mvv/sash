package com.github.mvv.sash.simple

import com.github.mvv.sash.EffectMacro

import scala.reflect.macros.blackbox.Context

object SimpleEffectMacro {
  final def effectImpl[A](c: Context)(body: c.Expr[A])(implicit bodyTag: c.WeakTypeTag[A]): c.Expr[A] = {
    import c.universe._
    def flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    EffectMacro.effectImpl(c)(predef = Seq.empty,
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
