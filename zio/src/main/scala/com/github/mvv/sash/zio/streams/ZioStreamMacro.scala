package com.github.mvv.sash.zio.streams

import com.github.mvv.sash.EffectMacro
import zio.stream.ZStream

import scala.reflect.macros.blackbox

object ZioStreamMacro {
  def streamImpl[R, E, A](c: blackbox.Context)(body: c.Expr[ZStream[R, E, A]])(
      implicit envTag: c.WeakTypeTag[R],
      errorTag: c.WeakTypeTag[E],
      resultTag: c.WeakTypeTag[A]): c.Expr[ZStream[R, E, A]] = {
    import c.universe._
    val unit = q"_root_.zio.stream.ZStream.succeed(())"
    val flatMap = { value: Tree =>
      q"$value.flatMap"
    }
    val raise = { value: Tree =>
      q"_root_.zio.stream.ZStream.fail($value)"
    }
    val bodyType =
      c.typecheck(q"(null: _root_.zio.stream.ZStream[${envTag.tpe}, ${errorTag.tpe}, ${resultTag.tpe}])").tpe
    EffectMacro.effectImpl(c)(predef = Seq.empty,
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
