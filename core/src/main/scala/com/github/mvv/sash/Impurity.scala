package com.github.mvv.sash

trait Impurity {
  @impurity
  final def impure(value: => Unit): Unit = outsideOfMacro
}
