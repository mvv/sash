# Sash

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
libraryDependencies += "com.github.mvv.sash" %% "sash" % "0.1-SNAPSHOT"
```

provides a "simple" version of the `effect` macro, which relies only on `flatMap` method. It can handle conditionals and loops,
but cannot handle try-catch-finally. The module also exposes the imlementation of the macro, which can be configured to handle
your favourite monads via a small compatibility layer. Sash comes with two such layers: one for the
[Cats](https://typelevel.org/cats) library

```scala
libraryDependencies += "com.github.mvv.sash" %% "sash-cats" % "0.1-SNAPSHOT"
```

and one for the [ZIO](https://github.com/scalaz/scalaz-zio)

```scala
libraryDependencies += "com.github.mvv.sash" %% "sash-zio" % "0.1-SNAPSHOT"
```
