package com.mhm.matryoshka.model

import scalaz.Functor

sealed trait MList[T]
final case class MCons[T](h:T, t:MList[T]) extends MList[T]
final case class MNil[T]() extends MList[T]

object MList {
  implicit val mListFunctor: Functor[MList] = new Functor[MList] {
    def map[A, B](fa: MList[A])(f: A => B): MList[B] = fa match {
      case MNil() => MNil[B]()
      case MCons(h, t) => MCons[B](f(h), map(t)(f))
    }
  }
}
