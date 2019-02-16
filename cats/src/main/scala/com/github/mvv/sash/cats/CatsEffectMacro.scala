package com.github.mvv.sash.cats

import com.github.mvv.sash.EffectMacro

import scala.reflect.macros.blackbox.Context
import scala.language.higherKinds

object CatsEffectMacro {
  def effectImpl[F[_], A](c: Context)(body: c.Expr[F[A]])(implicit witness: c.WeakTypeTag[F[_]]): c.Expr[F[A]] = {
    import c.universe._
    val cons = weakTypeTag[F[_]].tpe
    val monadInst = TermName(c.freshName("monad"))
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
    EffectMacro.effectImpl(c)(predef = predef,
                              unit = Some(unit),
                              flatMap = flatMap,
                              raise = Some(raise),
                              recover = Some(recover),
                              ensuring = None,
                              body = body)
  }
}
