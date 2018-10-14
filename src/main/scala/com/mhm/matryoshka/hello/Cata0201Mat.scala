package com.mhm.matryoshka.hello

import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import slamdata.Predef._

import scalaz.Functor

object Cata0201Mat extends App {

  sealed trait MList[A]
  final case class MCons[A](h:Int, t:A) extends MList[A]
  final case class MNil[A]() extends MList[A]

  implicit val mListFunctor: Functor[MList] = new Functor[MList] {
    def map[A, B](fa: MList[A])(f: A => B): MList[B] = fa match {
      case MNil() => MNil[B]()
      case MCons(h, t) => MCons[B](h, f(t))
    }
  }

  val mlist: Fix[MList] = Fix(MCons(1, Fix(MCons(4, Fix(MCons(7, Fix(MNil())))))))

  val printLabels: Algebra[MList, Unit] = {
    case MNil()   => println(s"visiting mnil")
    case MCons(h, _) => println(s"visiting $h")
  }

  mlist.cata(printLabels)

  val prod: Algebra[MList, Int] = {
    case MNil()   => 1
    case MCons(h, t) => h * t
  }

  println(mlist.cata(prod)) // 28


}
