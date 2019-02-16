package com.github.mvv.sash

import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context

object EffectMacro {
  private object MacroError extends RuntimeException

  final def effectImpl[A](c: Context)(predef: Seq[c.Tree],
                                      unit: Option[c.Tree],
                                      flatMap: c.Tree => c.Tree,
                                      raise: Option[c.Tree => c.Tree],
                                      recover: Option[(c.Tree, c.Tree) => c.Tree],
                                      ensuring: Option[(c.Tree, c.Tree) => c.Tree],
                                      body: c.Expr[A]): c.Expr[A] = {
    import c.universe._

    sealed trait NextStmt
    case class ConsumesResult(name: TermName, tpe: Type, cont: () => Tree) extends NextStmt
    case class IgnoresResult(cont: () => Tree) extends NextStmt
    case object Last extends NextStmt

    //System.err.println(s"INPUT: ${body.tree}")

    val effectfulTypeSymbol = typeOf[effectful].typeSymbol
    val impurityTypeSymbol = typeOf[impurity].typeSymbol
    val purityTypeSymbol = typeOf[purity].typeSymbol

    val processedMarkerName = TermName("__com_github_mvv_sash_processed__")
    def unitTree(pos: Position, msg: String): Tree = unit.getOrElse {
      c.error(pos, msg)
      throw MacroError
    }
    def recoverTree(pos: Position): (Tree, Tree) => Tree = recover.getOrElse {
      c.error(pos, "Macro is not configured to translate catches")
      throw MacroError
    }
    def ensuringTree(pos: Position): (Tree, Tree) => Tree = ensuring.getOrElse {
      c.error(pos, "Macro is not configured to translate finally")
      throw MacroError
    }
    def effectAnd(tree: Tree, next: NextStmt): Tree = next match {
      case ConsumesResult(name, tpe, cont) => q"${flatMap(tree)}(($name: $tpe) => ${cont()})"
      case IgnoresResult(cont)             => q"${flatMap(tree)}(_ => ${cont()})"
      case Last                            => tree
    }
    def pureAnd(tree: Tree, next: NextStmt): Tree = next match {
      case ConsumesResult(_, _, cont) =>
        c.error(tree.pos, "Bound non-effect")
        throw MacroError
      case IgnoresResult(cont) =>
        q"{ $tree; ${cont()} }"
      case Last =>
        c.error(tree.pos, "Non-effect at the end of the body")
        throw MacroError
    }
    object Effectful {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case Apply(fn @ Select(Apply(_, List(effect)), TermName("unary_$plus")), Nil) =>
          if (fn.symbol.annotations.exists(_.tree.tpe.typeSymbol == effectfulTypeSymbol)) {
            Some(effect)
          } else {
            None
          }
        case _ =>
          None
      }
    }
    object Impurity {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case Apply(fn, List(impurity)) =>
          if (fn.symbol.annotations.exists(_.tree.tpe.typeSymbol == impurityTypeSymbol)) {
            Some(impurity)
          } else {
            None
          }
        case _ =>
          None
      }
    }
    object Purity {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case Apply(fn, List(purity)) =>
          if (fn.symbol.annotations.exists(_.tree.tpe.typeSymbol == purityTypeSymbol)) {
            Some(purity)
          } else {
            None
          }
        case _ =>
          None
      }
    }
    def isProcessed(tree: Tree): Boolean = tree match {
      case q"{ $_ val ${`processedMarkerName`}: $_ = $_; ..$_ }" => true
      case _                                                     => false
    }
    object Stmts {
      def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = tree match {
        case Block(Nil, last)                                   => Some((last, Nil))
        case q"{ ..${Seq(first, rest @ _*)} }" if rest.nonEmpty => Some((first, rest))
        case _                                                  => None
      }
    }
    def handleExpr(expr: Tree, isStmt: Boolean = false)(cont: Tree => Tree): Tree = expr match {
      case Effectful(effect) =>
        val tempName = TermName(c.freshName())
        handleStmt(effect, ConsumesResult(tempName, expr.tpe, () => cont(Ident(tempName))))
      case Purity(purity) =>
        cont(purity)
      case _ if !isStmt && isProcessed(expr) =>
        cont(expr)
      case q"$subExpr.$name" =>
        handleExpr(subExpr) { subValue =>
          cont(treeCopy.Select(expr, subValue, name))
        }
      case q"$fn(..$args)" =>
        handleExpr(fn) { fnValue =>
          def handleArgs(unprocessed: Seq[Tree], processed: List[Tree]): Tree = unprocessed.headOption match {
            case Some(arg) =>
              handleExpr(arg) { argValue =>
                handleArgs(unprocessed.tail, argValue :: processed)
              }
            case None =>
              cont(treeCopy.Apply(expr, fnValue, processed.reverse))
          }
          handleArgs(args, Nil)
        }
      case q"$subExpr: $tp" =>
        handleExpr(subExpr) { subValue =>
          cont(treeCopy.Typed(expr, subValue, tp))
        }
      case _ =>
        cont(expr)
    }
    def handleStmts(stmt: Tree, rest: Seq[Tree], afterLast: NextStmt): Tree =
      handleStmt(stmt, rest.headOption match {
        case Some(nextStmt) => IgnoresResult(() => handleStmts(nextStmt, rest.tail, afterLast))
        case None           => afterLast
      })
    def handleStmt(stmt: Tree, next: NextStmt): Tree = stmt match {
      case q"$mods val $_: $_ = $_" if mods.hasFlag(Flag.LAZY) =>
        c.error(stmt.pos, "Lazy variables are not supported")
        throw MacroError
      case q"$mods val $name: $tp = ${expr @ Effectful(effect)}" =>
        val tempName = TermName(c.freshName())
        handleStmt(
          effect,
          ConsumesResult(tempName, expr.tpe, () => pureAnd(treeCopy.ValDef(stmt, mods, name, tp, q"$tempName"), next)))
      case q"$mods val $name: $tp = $expr" =>
        handleExpr(expr) { value =>
          pureAnd(treeCopy.ValDef(stmt, mods, name, tp, value), next)
        }
      case q"$_ var $_: $_ = $_" =>
        c.error(stmt.pos, "Mutable variables are not supported")
        throw MacroError
      case DefDef(_, _, _, _, _, _) =>
        pureAnd(stmt, next)
      case q"if ($cond) { ..${Seq()} } else { ..${Seq()} }" =>
        handleExpr(cond) { condValue =>
          val branchTree = unitTree(stmt.pos, "Macro is not configured to translate empty code blocks")
          effectAnd(q"if ($condValue) $branchTree else $branchTree", next)
        }
      case q"if ($cond) $whenTrue else { ..${Seq()} }" =>
        handleExpr(cond) { condValue =>
          next match {
            case _: ConsumesResult =>
              c.error(stmt.pos, "Unexpected binding")
              throw MacroError
            case IgnoresResult(cont) =>
              val restName = TermName(c.freshName("rest"))
              q"""{ def $restName = ${cont()}
                  ; if ($condValue) ${handleStmt(whenTrue, IgnoresResult(() => q"$restName"))} else $restName }"""
            case Last =>
              val elseTree = unitTree(stmt.pos, "Macro is not configured to translate empty code blocks")
              q"if ($condValue) ${handleStmt(whenTrue, Last)} else $elseTree"
          }
        }
      case q"if ($cond) $whenTrue else $whenFalse" =>
        handleExpr(cond) { condValue =>
          next match {
            case ConsumesResult(name, tpe, cont) =>
              val restName = TermName(c.freshName("rest"))
              val tempName = TermName(c.freshName())
              q"""{ def $restName($name: $tpe) = ${cont()}
                  ; if ($condValue)
                      ${handleStmt(whenTrue, ConsumesResult(tempName, tpe, () => q"$restName($tempName)"))}
                    else
                      ${handleStmt(whenFalse, ConsumesResult(tempName, tpe, () => q"$restName($tempName)"))} }"""
            case IgnoresResult(cont) =>
              val restName = TermName(c.freshName("rest"))
              q"""{ def $restName = ${cont()}
                  ; if ($condValue) ${handleStmt(whenTrue, IgnoresResult(() => q"$restName"))}
                    else ${handleStmt(whenFalse, IgnoresResult(() => q"$restName"))} }"""
            case Last =>
              q"if ($condValue) ${handleStmt(whenTrue, Last)} else ${handleStmt(whenFalse, Last)}"
          }
        }
      case q"$expr match { case ..$cases }" =>
        handleExpr(expr) { value =>
          next match {
            case ConsumesResult(name, tpe, cont) =>
              val restName = TermName(c.freshName("rest"))
              val handledCases = cases.map {
                case cs @ CaseDef(pat, guard, subStmt) =>
                  val tempName = TermName(c.freshName())
                  val caseNext = ConsumesResult(tempName, tpe, () => q"$restName($tempName)")
                  treeCopy.CaseDef(cs, pat, guard, handleStmt(subStmt, caseNext))
              }
              q"""{ def $restName($name: $tpe) = ${cont()}
                  ; $value match { case ..$handledCases } }"""
            case IgnoresResult(cont) =>
              val restName = TermName(c.freshName("rest"))
              val handledCases = cases.map {
                case cs @ CaseDef(pat, guard, subStmt) =>
                  treeCopy.CaseDef(cs, pat, guard, handleStmt(subStmt, IgnoresResult(() => q"$restName")))
              }
              q"""{ def $restName = ${cont()}
                  ; $value match { case ..$handledCases } }"""
            case Last =>
              val handledCases = cases.map {
                case cs @ CaseDef(pat, guard, subStmt) =>
                  treeCopy.CaseDef(cs, pat, guard, handleStmt(subStmt, Last))
              }
              q"$value match { case ..$handledCases }"
          }
        }
      case q"while ($cond) $whileTrue" =>
        val loopName = TermName(c.freshName("loop"))
        val done = next match {
          case _: ConsumesResult =>
            c.error(stmt.pos, "Unexpected binding")
            throw MacroError
          case IgnoresResult(cont) => cont()
          case Last                => unitTree(stmt.pos, "Macro is not configured to translate loops")
        }
        val loopBody = handleExpr(cond) { condValue =>
          q"if ($condValue) ${handleStmt(whileTrue, IgnoresResult(() => q"$loopName"))} else $done"
        }
        q"{ def $loopName: ${done.tpe} = $loopBody; $loopName }"
      case q"do $whileTrue while($cond)" =>
        val loopName = TermName(c.freshName("loop"))
        val done = next match {
          case _: ConsumesResult =>
            c.error(stmt.pos, "Unexpected binding")
            throw MacroError
          case IgnoresResult(cont) => cont()
          case Last                => unitTree(stmt.pos, "Macro is not configured to translate loops")
        }
        val loopBody = handleStmt(whileTrue, IgnoresResult { () =>
          handleExpr(cond) { condValue =>
            q"if ($condValue) $loopName else $done"
          }
        })
        q"{ def $loopName: ${done.tpe} = $loopBody; $loopName }"
      case q"throw $expr" =>
        handleExpr(expr) { value =>
          val raiseTree = raise.getOrElse {
            c.error(stmt.pos, "Macro is not configured to translate throws")
            throw MacroError
          }
          effectAnd(raiseTree(value), next)
        }
      case q"try $tryStmt finally $finallyStmt" =>
        effectAnd(ensuringTree(stmt.pos)(handleStmt(tryStmt, Last), handleStmt(finallyStmt, Last)), next)
      case q"try $tryStmt catch { case ..$catchCases }" =>
        val handledTry = handleStmt(tryStmt, Last)
        val handledCases = catchCases.map {
          case cs @ CaseDef(pat, guard, subStmt) =>
            treeCopy.CaseDef(cs, pat, guard, handleStmt(subStmt, Last))
        }
        effectAnd(recoverTree(stmt.pos)(handledTry, q"{ case ..$handledCases }"), next)
      case q"try $tryStmt catch { case ..$catchCases } finally $finallyStmt" =>
        val handledTry = handleStmt(tryStmt, Last)
        val handledCases = catchCases.map {
          case cs @ CaseDef(pat, guard, subStmt) =>
            treeCopy.CaseDef(cs, pat, guard, handleStmt(subStmt, Last))
        }
        val handledFinally = handleStmt(finallyStmt, Last)
        effectAnd(
          ensuringTree(stmt.pos)(recoverTree(stmt.pos)(handledTry, q"{ case ..$handledCases }"), handledFinally),
          next)
      case q"()" =>
        effectAnd(unitTree(stmt.pos, "Macro is not configured to translate empty code blocks"), next)
      case q"{ ..${Seq()} }" =>
        effectAnd(unitTree(stmt.pos, "Macro is not configured to translate empty code blocks"), next)
      case _ if isProcessed(stmt) =>
        effectAnd(stmt, next)
      case Stmts(subStmt, rest) =>
        next match {
          case ConsumesResult(name, tpe, cont) =>
            val restName = TermName(c.freshName("rest"))
            val tempName = TermName(c.freshName())
            q"""{ def $restName($name: $tpe) = ${cont()}
                ; ${handleStmts(subStmt, rest, ConsumesResult(tempName, tpe, () => q"$restName($tempName)"))} }"""
          case IgnoresResult(cont) =>
            val restName = TermName(c.freshName("rest"))
            q"""{ def $restName = ${cont()}
                ; ${handleStmts(subStmt, rest, IgnoresResult(() => q"$restName"))} }"""
          case Last =>
            handleStmts(subStmt, rest, next)
        }
      case TypeDef(_, _, _, _) =>
        pureAnd(stmt, next)
      case ClassDef(_, _, _, _) =>
        pureAnd(stmt, next)
      case ModuleDef(_, _, _) =>
        pureAnd(stmt, next)
      case Import(_, _) =>
        pureAnd(stmt, next)
      case Return(_) =>
        c.error(stmt.pos, "Return statements are not supported")
        throw MacroError
      case Impurity(impurity) =>
        pureAnd(impurity, next)
      case _ =>
        handleExpr(stmt, isStmt = true) { value =>
          effectAnd(value, next)
        }
    }
    val bodyTree = body.tree
    val result = try {
      handleStmt(bodyTree, Last)
    } catch {
      case MacroError => return body
    }
    val withPredef = q"{ val $processedMarkerName = (); ..$predef; $result }"
    val untyped = c.untypecheck(withPredef)
    //System.err.println(s"UNTYPED: $untyped")
    val typed = try {
      c.typecheck(untyped)
    } catch {
      case e: TypecheckException =>
        c.error(e.pos.asInstanceOf[Position], e.msg)
        return body
    }
    //System.err.println(s"TYPED: $typed")
    c.Expr[A](typed)
  }
}
