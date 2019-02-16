package com.github.mvv.sash

trait Purity {
  @purity
  def pure[A](value: => A): A = outsideOfMacro
}
