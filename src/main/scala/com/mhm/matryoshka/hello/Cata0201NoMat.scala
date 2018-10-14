package com.mhm.matryoshka.hello

import com.mhm.matryoshka.model.{MCons, MList, MNil}


/**
  * catamorphism done without Matryoshka
  * based on lex, section 2.1, the list catamorphism
  *
  * lex - "Sorting morphisms", by Lex Augusteijn, Eindhoven
  */


/**

Equivalent Haskell:

type List_cata x u = (u, x->u->u)

list_cata :: List_cata x u -> [x] -> u
list_cata (a,f) = cata where
  cata [] = a
  cata (x:l) = f x (cata l)

 */

/**
  * lex 2.1 without Matryoshka
  */
object Cata0201NoMat extends App {

  case class List_cata[X,U](a: U, f:(X,U) => U)

  def list_cata[X,U](lc: List_cata[X,U])(l:MList[X]): U = {
    l match {
      case MNil => lc.a
      case x:MCons[X] => lc.f(x.h, list_cata(lc)(x.t))
    }
  }

  def prod = list_cata[Int,Int](List_cata(1, _ * _ )) _

  val lst = MCons[Int](1, MCons[Int](3, MCons[Int](9, MNil)))

  println(prod(lst)) // 27

}
