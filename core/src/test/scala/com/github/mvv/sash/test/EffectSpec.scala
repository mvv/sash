package com.github.mvv.sash.test

import com.github.mvv.sash.simple._
import org.specs2.mutable.Specification

class EffectSpec extends Specification {
  import TM.Trace._

  "Effect macro should" >> {
    "handle pure bindings" >> {
      effect {
        val x = 1
        TM(x + x)
      }.trace shouldEqual Done(2)
    }

    "handle effects in variable bindings" >> {
      effect {
        val x = 2 + +TM(1)
        TM(x + x)
      }.trace shouldEqual Push(Pop(1, Done(6)))
    }

    "handle effectful blocks in variable bindings" >> {
      effect {
        val x = + {
          TM.say("foo")
          TM(1)
        }
        TM(x + x)
      }.trace shouldEqual Push(Said("foo", Pop((), Push(Pop(1, Done(2))))))
    }

    "handle effects in conditions" >> {
      effect {
        val x = 1
        val y = if (x < +TM(2)) "foo" else "bar"
        TM.say(y)
      }.trace shouldEqual Push(Pop(2, Said("foo", Done(()))))
    }

    "handle effects in matched values" >> {
      effect {
        val x = 1
        val y = +TM(2) + x match {
          case 3 => 10
          case _ => 11
        }
        TM(y)
      }.trace shouldEqual Push(Pop(2, Done(10)))
    }

    "handle effects in statements" >> {
      effect {
        TM((+TM(1), +TM(2)))
      }.trace shouldEqual Push(Pop(1, Push(Pop(2, Done((1, 2))))))
    }

    "handle effects in typed expressions" >> {
      effect {
        TM(+TM(1): Int)
      }.trace shouldEqual Push(Pop(1, Done(1)))
    }

    "handle statement join" >> {
      val trace = effect {
        +TM(TM(1))
      }.trace
      trace match {
        case Push(Pop(_, Done(1))) => ok
        case _                     => ko
      }
    }

    "handle subblocks of statements" >> {
      effect {
        TM.say("before")
        ({
          val x = +TM(1)
          TM(x + 2)
        })
        TM.say("after")
      }.trace shouldEqual Push(Said("before", Pop((), Push(Pop(1, Push(Pop(3, Said("after", Done(())))))))))
    }

    "handle if-then-else" >> {
      effect {
        val x = 1
        if (x < +TM(2)) {
          TM(10)
        } else {
          TM(11)
        }
      }.trace shouldEqual Push(Pop(2, Done(10)))
    }

    "handle if-then" >> {
      effect {
        val x = 1
        if (x < +TM(2)) {
          TM.say("true")
        }
        TM.say("after")
      }.trace shouldEqual Push(Pop(2, Push(Said("true", Pop((), Said("after", Done(())))))))
    }

    "handle while loops" >> {
      var counter = 2
      effect {
        while (counter > +TM(0)) {
          TM.say(s"counter-$counter")
          impure { counter -= 1 }
        }
        TM(())
      }.trace shouldEqual Push(Pop(0, Push(Said("counter-2", Pop((),
                            Push(Pop(0, Push(Said("counter-1", Pop((),
                              Push(Pop(0, Done(())))))))))))))
    }

    "handle do-while loops" >> {
      var counter = 2
      effect {
        do {
          TM.say(s"counter-$counter")
          impure { counter -= 1 }
        } while (counter > +TM(0))
        TM(())
      }.trace shouldEqual Push(Said("counter-2", Pop((), Push(Pop(0,
                            Push(Said("counter-1", Pop((), Push(Pop(0,
                              Done(())))))))))))
    }

    "handle matches" >> {
      effect {
        +TM(2) match {
          case 1 =>
            TM.say("one")
          case x if x > 0 =>
            TM.say(x.toString)
            TM.say("two")
        }
        TM.say("end")
      }.trace shouldEqual Push(Pop(2, Push(Said("2", Pop((), Push(Said("two", Pop((),
                            Said("end", Done(()))))))))))
    }

    "respect purity" >> {
      effect {
        TM.say("start")
        pure {
          TM.say("lost")
          TM.say("end")
        }
      }.trace shouldEqual Push(Said("start", Pop((), Said("end", Done(())))))
    }

    "handle nested macro applications" >> {
      effect {
        val x = 1
        TM.say("start")
        effect {
          if (x < +TM(2)) {
            TM.say("true")
          }
          TM(( ))
        }
        TM.say("end")
      }.trace shouldEqual Push(Said("start", Pop((),
                            Push(Push(Pop(2, Push(Said("true", Pop((), Pop((), Said("end", Done(()))))))))))))
    }

    "handle patterns in variable bindings" >> {
      effect {
        val (x, y) = (+TM(1), +TM(2))
        TM(x + y)
      }.trace shouldEqual Push(Pop(1, Push(Pop(2, Done(3)))))
    }

    "handle patterns with type parameters in unapply" >> {
      object Foo {
        def unapply(x: Any): Option[Any] = Some(x)
      }
      object Bar {
        def unapply[A](x: Any): Option[Any] = Some(x)
      }
      effect {
        TM(1).flatMap {
          case Bar(x) => TM(x)
          case Foo(y @ Bar(_)) => TM(y)
        }
      }.trace shouldEqual Push(Pop(1, Done(1)))
    }

    "handle patterns with unapplySeq" >> {
      effect {
        TM(List(1, 2)).flatMap {
          case Seq(x, y) => TM(x + y)
          case _ => TM.raise(new RuntimeException)
        }
      }.trace shouldEqual Push(Pop(List(1, 2), Done(3)))
    }
  }
}
