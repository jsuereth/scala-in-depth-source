package com.github.jsuereth.applicative

trait Monoid[A] {
  def zero: A
  def combine(a: A, b: A): A
}

object Monoid {}