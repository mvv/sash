package com.github.mvv.sash

import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context

abstract class EffectMacro {
  val c: Context

  import c.universe._

  private object MacroError extends RuntimeException

  private val effectfulTypeSymbol = typeOf[effectful].typeSymbol
  private val impurityTypeSymbol = typeOf[impurity].typeSymbol
  private val purityTypeSymbol = typeOf[purity].typeSymbol

  private object Effectful {
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

  private object Impurity {
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

  private object Purity {
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

  private val unapplyName = TermName("unapply")
  private val unapplySeqName = TermName("unapply")
  private val SELECTOR_DUMMY = TermName("<unapply-selector>")

  private def stripUnApplyFromPat(pat: Tree): Tree = pat match {
    case UnApply(app @ Apply(TypeApply(Select(qual, `unapplyName` | `unapplySeqName`), _), List(Ident(SELECTOR_DUMMY))),
                 args) =>
      treeCopy.Apply(app, qual, args.map(stripUnApplyFromPat))
    case UnApply(fn, args) =>
      treeCopy.UnApply(pat, fn, args.map(stripUnApplyFromPat))
    case Apply(fn, args) =>
      treeCopy.Apply(pat, fn, args.map(stripUnApplyFromPat))
    case Bind(name, subPat) =>
      treeCopy.Bind(pat, name, stripUnApplyFromPat(subPat))
    case _ =>
      pat
  }

  private def stripUnApplyFromCase(recur: Tree => Tree)(cs: CaseDef): CaseDef = cs match {
    case CaseDef(pat, guard, whenMatches) =>
      CaseDef(stripUnApplyFromPat(pat), stripUnApply(guard), recur(whenMatches))
  }

  private def stripUnApply(expr: Tree): Tree = expr match {
    case Select(subExpr, name) =>
      Select(stripUnApply(subExpr), name)
    case Apply(fn, args) =>
      Apply(stripUnApply(fn), args.map(stripUnApply))
    case Function(params, subExpr) =>
      Function(params, stripUnApply(subExpr))
    case TypeApply(fn, args) =>
      TypeApply(stripUnApply(fn), args)
    case Typed(subExpr, tpe) =>
      Typed(stripUnApply(subExpr), tpe)
    case Block(stmts, subExpr) =>
      Block(stmts.map(stripUnApply), stripUnApply(subExpr))
    case If(cond, whenTrue, whenFalse) =>
      If(stripUnApply(cond), stripUnApply(whenTrue), stripUnApply(whenFalse))
    case Match(subExpr, cases) =>
      Match(stripUnApply(subExpr), cases.map(stripUnApplyFromCase(stripUnApply)))
    case ValDef(mods, name, tp, subExpr) =>
      treeCopy.ValDef(expr, mods, name, tp, stripUnApply(subExpr))
    case DefDef(mods, name, typeParams, params, tp, subExpr) =>
      treeCopy.DefDef(expr, mods, name, typeParams, params, tp, stripUnApply(subExpr))
    case q"while ($cond) $subExpr" =>
      q"while (${stripUnApply(cond)}) ${stripUnApply(subExpr)}"
    case q"do $subExpr while ($cond)" =>
      q"do ${stripUnApply(subExpr)} while (${stripUnApply(cond)})"
    case Throw(subExpr) =>
      Throw(stripUnApply(subExpr))
    case Try(block, catches, finalizer) =>
      Try(stripUnApply(block), catches.map(stripUnApplyFromCase(stripUnApply)), stripUnApply(finalizer))
    case Return(subExpr) =>
      Return(stripUnApply(subExpr))
    case _ =>
      expr
  }

  private val processedMarkerName = TermName("__com_github_mvv_sash_processed__")

  private def isProcessed(tree: Tree): Boolean = tree match {
    case q"{ $_ val ${`processedMarkerName`}: $_ = $_; ..$_ }" => true
    case _                                                     => false
  }

  private object Stmts {
    def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = tree match {
      case Block(Nil, last)                                   => Some((last, Nil))
      case q"{ ..${Seq(first, rest @ _*)} }" if rest.nonEmpty => Some((first, rest))
      case _                                                  => None
    }
  }

  final def effectImpl[A](predef: Seq[Tree],
                          unit: Option[Tree],
                          flatMap: Tree => Tree,
                          raise: Option[Tree => Tree],
                          recover: Option[(Tree, Tree) => Tree],
                          ensuring: Option[(Tree, Tree) => Tree],
                          ensuringType: Option[Type],
                          body: Expr[A],
                          bodyType: Type): Expr[A] = {
    sealed trait NextStmt {
      def contType: Type
    }
    final case class ConsumesResult(bindName: TermName, bindType: Type, cont: () => Tree, contType: Type)
        extends NextStmt
    final case class IgnoresResult(cont: () => Tree, contType: Type) extends NextStmt
    final case class Last(contType: Type) extends NextStmt

    //System.err.println(s"INPUT: ${body.tree}")

    def getUnitTree(pos: Position): Tree = unit.getOrElse {
      c.error(pos, "Macro is not configured to translate empty code blocks")
      throw MacroError
    }
    def getEnsuringType(pos: Position): Type = ensuringType.getOrElse {
      c.error(pos, "Macro is not configured to translate finally")
      throw MacroError
    }
    def getRecoverTree(pos: Position): (Tree, Tree) => Tree = recover.getOrElse {
      c.error(pos, "Macro is not configured to translate catches")
      throw MacroError
    }
    def getEnsuringTree(pos: Position): (Tree, Tree) => Tree = ensuring.getOrElse {
      c.error(pos, "Macro is not configured to translate finally")
      throw MacroError
    }
    def effectAnd(tree: Tree, next: NextStmt): Tree = next match {
      case ConsumesResult(bindName, bindType, cont, _) => q"${flatMap(tree)}(($bindName: $bindType) => ${cont()})"
      case IgnoresResult(cont, _)                      => q"${flatMap(tree)}(_ => ${cont()})"
      case Last(contType)                              => q"$tree: $contType"
    }
    def pureAnd(tree: Tree, next: NextStmt): Tree = next match {
      case _: ConsumesResult =>
        c.error(tree.pos, "Unexpected binding")
        throw MacroError
      case IgnoresResult(cont, _) =>
        q"{ $tree; ${cont()} }"
      case Last(_) =>
        c.error(tree.pos, "Non-effect at the end of a code block")
        throw MacroError
    }

    def handleExpr(expr: Tree, contType: Type, isStmt: Boolean = false)(cont: Tree => Tree): Tree = expr match {
      case Effectful(effect) =>
        val tempName = TermName(c.freshName())
        handleStmt(effect, ConsumesResult(tempName, expr.tpe, () => cont(Ident(tempName)), contType))
      case Purity(purity) =>
        cont(stripUnApply(purity))
      case _ if !isStmt && isProcessed(expr) =>
        cont(stripUnApply(expr))
      case Select(subExpr, name) =>
        handleExpr(subExpr, contType) { subValue =>
          cont(Select(subValue, name))
        }
      case Apply(fn, args) =>
        handleExpr(fn, contType) { fnValue =>
          def handleArgs(unprocessed: Seq[Tree], processed: List[Tree]): Tree = unprocessed.headOption match {
            case Some(arg) =>
              handleExpr(arg, contType) { argValue =>
                handleArgs(unprocessed.tail, argValue :: processed)
              }
            case None =>
              cont(Apply(fnValue, processed.reverse))
          }
          handleArgs(args, Nil)
        }
      case TypeApply(fn, args) =>
        handleExpr(fn, contType) { fnValue =>
          cont(TypeApply(fnValue, args))
        }
      case Typed(subExpr, tp) =>
        handleExpr(subExpr, contType) { subValue =>
          cont(Typed(subValue, tp))
        }
      case If(cond, whenTrue, whenFalse) =>
        handleExpr(cond, contType) { condValue =>
          cont(If(condValue, stripUnApply(whenTrue), stripUnApply(whenFalse)))
        }
      case Match(subExpr, cases) =>
        handleExpr(subExpr, contType) { subValue =>
          cont(Match(subValue, cases.map(stripUnApplyFromCase(stripUnApply))))
        }
      case _ =>
        cont(stripUnApply(expr))
    }
    def handleStmts(stmt: Tree, rest: Seq[Tree], afterLast: NextStmt): Tree =
      handleStmt(stmt, rest.headOption match {
        case Some(nextStmt) => IgnoresResult(() => handleStmts(nextStmt, rest.tail, afterLast), afterLast.contType)
        case None           => afterLast
      })
    def handleStmt(stmt: Tree, next: NextStmt): Tree = stmt match {
      case q"$mods val $_: $_ = $_" if mods.hasFlag(Flag.LAZY) =>
        c.error(stmt.pos, "Lazy variables are not supported")
        throw MacroError
      case q"$mods val $name: $tp = ${expr @ Effectful(effect)}" =>
        val tempName = TermName(c.freshName())
        handleStmt(effect,
                   ConsumesResult(tempName,
                                  expr.tpe,
                                  () => pureAnd(treeCopy.ValDef(stmt, mods, name, tp, q"$tempName"), next),
                                  next.contType))
      case q"$mods val $name: $tp = $expr" =>
        handleExpr(expr, next.contType) { value =>
          pureAnd(treeCopy.ValDef(stmt, mods, name, tp, value), next)
        }
      case q"$_ var $_: $_ = $_" =>
        c.error(stmt.pos, "Mutable variables are not supported")
        throw MacroError
      case DefDef(mods, name, typeParams, params, tp, expr) =>
        pureAnd(treeCopy.DefDef(stmt, mods, name, typeParams, params, tp, stripUnApply(expr)), next)
      case q"if ($cond) { ..${Seq()} } else { ..${Seq()} }" =>
        handleExpr(cond, next.contType) { condValue =>
          val branchTree = getUnitTree(stmt.pos)
          effectAnd(q"if ($condValue) $branchTree else $branchTree", next)
        }
      case q"if ($cond) $whenTrue else { ..${Seq()} }" =>
        handleExpr(cond, next.contType) { condValue =>
          next match {
            case _: ConsumesResult =>
              c.error(stmt.pos, "Unexpected binding")
              throw MacroError
            case IgnoresResult(cont, contType) =>
              val restName = TermName(c.freshName("rest"))
              q"""{ def $restName = ${cont()}
                  ; if ($condValue) ${handleStmt(whenTrue, IgnoresResult(() => q"$restName", contType))}
                    else $restName }"""
            case Last(_) =>
              c.error(stmt.pos, "Partial conditional at the end of a code block")
              throw MacroError
          }
        }
      case q"if ($cond) $whenTrue else $whenFalse" =>
        handleExpr(cond, next.contType) { condValue =>
          val (decls, newNext) = next match {
            case ConsumesResult(bindName, bindType, cont, contType) =>
              val restName = TermName(c.freshName("rest"))
              val tempName = TermName(c.freshName())
              val restNext = ConsumesResult(tempName, bindType, () => q"$restName($tempName)", contType)
              (Seq(q"def $restName($bindName: $bindType) = ${cont()}"), restNext)
            case IgnoresResult(cont, contType) =>
              val restName = TermName(c.freshName("rest"))
              val restNext = IgnoresResult(() => q"$restName", contType)
              (Seq(q"def $restName = ${cont()}"), restNext)
            case _: Last =>
              (Seq.empty, next)
          }
          val handled = q"if ($condValue) ${handleStmt(whenTrue, newNext)} else ${handleStmt(whenFalse, newNext)}"
          q"{ ..${decls :+ handled} }"
        }
      case q"$expr match { case ..$cases }" =>
        handleExpr(expr, next.contType) { value =>
          val (decls, handledCases) = next match {
            case ConsumesResult(bindName, bindType, cont, contType) =>
              val restName = TermName(c.freshName("rest"))
              val handledCases = cases.map(stripUnApplyFromCase { subStmt =>
                val tempName = TermName(c.freshName())
                val caseNext = ConsumesResult(tempName, bindType, () => q"$restName($tempName)", contType)
                handleStmt(subStmt, caseNext)
              })
              (List(q"def $restName($bindName: $bindType) = ${cont()}"), handledCases)
            case IgnoresResult(cont, contType) =>
              val restName = TermName(c.freshName("rest"))
              val handledCases =
                cases.map(stripUnApplyFromCase(handleStmt(_, IgnoresResult(() => q"$restName", contType))))
              (List(q"def $restName = ${cont()}"), handledCases)
            case Last(contType) =>
              val handledCases = cases.map(stripUnApplyFromCase(handleStmt(_, Last(contType))))
              (Nil, handledCases)
          }
          val handled = q"$value match { case ..$handledCases }"
          q"{ ..${decls :+ handled} }"
        }
      case q"while ($cond) $whileTrue" =>
        val loopName = TermName(c.freshName("loop"))
        val done = next match {
          case _: ConsumesResult =>
            c.error(stmt.pos, "Unexpected binding")
            throw MacroError
          case IgnoresResult(cont, _) =>
            cont()
          case Last(_) =>
            c.error(stmt.pos, "Loop at the end of a code block")
            throw MacroError
        }
        val loopBody = handleExpr(cond, next.contType) { condValue =>
          q"if ($condValue) ${handleStmt(whileTrue, IgnoresResult(() => q"$loopName", next.contType))} else $done"
        }
        q"{ def $loopName: ${next.contType} = $loopBody; $loopName }"
      case q"do $whileTrue while($cond)" =>
        val loopName = TermName(c.freshName("loop"))
        val done = next match {
          case _: ConsumesResult =>
            c.error(stmt.pos, "Unexpected binding")
            throw MacroError
          case IgnoresResult(cont, _) =>
            cont()
          case Last(_) =>
            c.error(stmt.pos, "Loop at the end of a code block")
            throw MacroError
        }
        val loopBody = handleStmt(whileTrue, IgnoresResult({ () =>
          handleExpr(cond, next.contType) { condValue =>
            q"if ($condValue) $loopName else $done"
          }
        }, next.contType))
        q"{ def $loopName: ${next.contType} = $loopBody; $loopName }"
      case Throw(expr) =>
        handleExpr(expr, next.contType) { value =>
          val raiseTree = raise.getOrElse {
            c.error(stmt.pos, "Macro is not configured to translate throws")
            throw MacroError
          }
          effectAnd(raiseTree(value), next)
        }
      case q"try $tryStmt finally $finallyStmt" =>
        val finallyType = getEnsuringType(stmt.pos)
        effectAnd(getEnsuringTree(stmt.pos)(handleStmt(tryStmt, Last(next.contType)),
                                            handleStmt(finallyStmt, Last(finallyType))),
                  next)
      case q"try $tryStmt catch { case ..$catchCases }" =>
        val handledTry = handleStmt(tryStmt, Last(next.contType))
        val handledCases = catchCases.map(stripUnApplyFromCase(handleStmt(_, Last(next.contType))))
        effectAnd(getRecoverTree(stmt.pos)(handledTry, q"{ case ..$handledCases }"), next)
      case q"try $tryStmt catch { case ..$catchCases } finally $finallyStmt" =>
        val handledTry = handleStmt(tryStmt, Last(next.contType))
        val handledCases = catchCases.map(stripUnApplyFromCase(handleStmt(_, Last(next.contType))))
        val finallyType = getEnsuringType(stmt.pos)
        val handledFinally = handleStmt(finallyStmt, Last(finallyType))
        effectAnd(
          getEnsuringTree(stmt.pos)(getRecoverTree(stmt.pos)(handledTry, q"{ case ..$handledCases }"), handledFinally),
          next)
      case q"()" =>
        effectAnd(getUnitTree(stmt.pos), next)
      case q"{ ..${Seq()} }" =>
        effectAnd(getUnitTree(stmt.pos), next)
      case _ if isProcessed(stmt) =>
        effectAnd(stmt, next)
      case Stmts(subStmt, rest) =>
        val (decls, newNext) = next match {
          case ConsumesResult(bindName, bindType, cont, contType) =>
            val restName = TermName(c.freshName("rest"))
            val tempName = TermName(c.freshName())
            val restNext = ConsumesResult(tempName, bindType, () => q"$restName($tempName)", contType)
            (List(q"def $restName($bindName: $bindType) = ${cont()}"), restNext)
          case IgnoresResult(cont, contType) =>
            val restName = TermName(c.freshName("rest"))
            val restNext = IgnoresResult(() => q"$restName", contType)
            (List(q"def $restName = ${cont()}"), restNext)
          case Last(_) =>
            (Nil, next)
        }
        q"{ ..${decls :+ handleStmts(subStmt, rest, newNext)} }"
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
        pureAnd(stripUnApply(impurity), next)
      case _ =>
        handleExpr(stmt, next.contType, isStmt = true) { value =>
          effectAnd(value, next)
        }
    }
    val bodyTree = body.tree
    val result = try {
      handleStmt(bodyTree, Last(bodyType))
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
