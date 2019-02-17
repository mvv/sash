# Sash
[![Release Version](https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.mvv.sash/sash_2.12.svg)](https://oss.sonatype.org/content/repositories/releases/com/github/mvv/sash)
[![Snapshot Version](https://img.shields.io/nexus/s/https/oss.sonatype.org/com.github.mvv.sash/sash_2.12.svg)](https://oss.sonatype.org/content/repositories/snapshots/com/github/mvv/sash)
[![Build Status](https://travis-ci.com/mvv/sash.svg?branch=master)](https://travis-ci.com/mvv/sash)

Sash translates regular Scala code into monadic expressions via a blackbox (meaning that it is mostly transparent to your IDE
typechecker) macro. Unlike some alternatives, Sash clearly splits the code it translates into statements, which are chained
together via some version of `flatMap`, and expressions, which are searched for effectful subexpressions. This approach
eliminates the need for the infamous `_ <- EFFECT` construct or its equivalents:

```scala
import com.github.mvv.sash.zio._
import scalaz.zio._
import scalaz.zio.console._
import java.io.IOException

def demand(thing: String): IO[IOException, Unit] = effect[IOException] {
  def ask = effect {
    try getStrLn
    catch {
      case e: IOException =>
        putStrLn("Yikes!")
        throw e
    }
  }
  putStrLn(s"Enter $thing")
  val answer = +Ref.make(+ask)
  while(+answer.get != thing) {
    putStrLn(s"No, enter $thing")
    answer.set(+ask)
  }
  putStrLn("You pass")
}
```

The only noticeable difference from a regular Scala code is the prolific use of `+EFFECT` construct, which is roughly
equivalent to `VAL <- EFFECT` in for-comprehensions. For example

```scala
val answer = +Ref.make(+ask)
```

would look like

```scala
tmp    <- ask
answer <- Ref.make(tmp)
```
and

```scala
answer.set(+ask)
```

would look like

```scala
tmp <- ask
_   <- answer.set(tmp)
```

inside a `for`.

## Using Sash in your project

Core Sash module

```scala
libraryDependencies += "com.github.mvv.sash" %% "sash" % "0.1-M1"
```

provides a "simple" version of the `effect` macro, which relies only on `flatMap` method. It can handle conditionals and loops,
but cannot handle try-catch-finally. The module also exposes the imlementation of the macro, which can be configured to handle
your favourite monads via a small compatibility layer. Sash comes with two such layers: one for the
[Cats](https://typelevel.org/cats) library

```scala
libraryDependencies += "com.github.mvv.sash" %% "sash-cats" % "0.1-M1"
```

and one for the [ZIO](https://github.com/scalaz/scalaz-zio)

```scala
libraryDependencies += "com.github.mvv.sash" %% "sash-zio" % "0.1-M1"
```

## Translation rules

Translation starts with the argument of the macro, which is treated as a statement. A statement can be

  * A unit value `()`
  * A block `{ [STMT]* }` of statements
  * A conditional `if (EXPR) STMT [else STMT]`
  * A match `EXPR match { [case ... => STMT]* }`
  * A loop `while (EXPR) STMT` or `do STMT while (EXPR)`
  * A variable declaration `[implicit] val NAME[: TYPE] = EXPR`
  * An error raising statement `throw EXPR`
  * An error handling statement `try STMT [catch { [case ... => STMT]* }] [finally STMT]`
  * An import or a type/class/trait/object/function definition. Those are left left as-is, meaning that they are simply brought
    into scope of the subsequent statements.
  * Impure code `impure CODE`, where `CODE` is a regular Scala code
  * An expression `EXPR`

Expressions `EXPR` are analyzed further, to see if they are

  * Effectful `+STMT`
  * Pure `pure CODE`, where `CODE` is a regular Scala code
  * Typed `EXPR: TYPE`
  * An application `EXPR([EXPR]*)`
  * An accessor `EXPR.NAME`
  * A conditional `if (EXPR) CODE else CODE`
  * A match `EXPR match { [case ... => CODE] }`
  * A regular Scala code `CODE`
