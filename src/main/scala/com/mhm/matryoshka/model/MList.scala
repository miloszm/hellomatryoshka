package com.mhm.matryoshka.model

import scalaz.Functor

sealed trait MList[+T]

final case class MCons[T](h:T, t:MList[T]) extends MList[T]
final case object MNil extends MList[Nothing]

object MList {
  implicit val mListFunctor: Functor[MList] = new Functor[MList] {
    def map[A, B](fa: MList[A])(f: A => B): MList[B] = fa match {
      case MNil => MNil
      case MCons(h, t) => MCons[B](f(h), map(t)(f))
    }
  }
  def findMin(l: MList[Int]): Option[Int] = l match {
    case MNil => None
    case MCons(e, MNil) => Some(e)
    case MCons(h,MCons(h2, t)) => findMin(MCons(Math.min(h,h2), t))
  }
  def remove(x:Int, l: MList[Int]): MList[Int] = l match {
    case MNil => MNil
    case MCons(h,t) => if (h == x) t else MCons(h, remove(x, t))
  }

}
