package com.mhm.matryoshka.hello


object HelloListCataSimple extends App {


  sealed trait MList[T]
  final case class MCons[T](h:T, t:MList[T]) extends MList[T]
  final case class MNil[T]() extends MList[T]

  type Pair[A,B] = (A,B)
  type List_cata[X,U] = Pair[U, (X,U) => U]

  def list_cata[X,U](lc: List_cata[X,U])(l:MList[X]): U = {
    l match {
      case _:MNil[X] => lc._1
      case x:MCons[X] => lc._2(x.h, list_cata(lc)(x.t))
    }
  }

  def prod = list_cata[Int,Int]((1, _ * _ )) _

  val lst = MCons[Int](1, MCons[Int](3, MCons[Int](9, MNil[Int]())))

  println(prod(lst))


}
