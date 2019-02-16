package com.github.mvv

import scala.annotation.StaticAnnotation

package object sash {
  final class effectful extends StaticAnnotation
  final class impurity extends StaticAnnotation
  final class purity extends StaticAnnotation
  def outsideOfMacro: Nothing = throw new RuntimeException("Used outside of a macro")
}
