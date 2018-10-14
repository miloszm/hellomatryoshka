package com.mhm.matryoshka.hello

import com.mhm.matryoshka.model.{MCons, MList, MNil}


/**

Equivalent Haskell:

  insertion_sort l = list_cata([],insert) l where
    insert x [] = [x]
    insert x (a:l)  | x < a = x:a:l
                    | otherwise = a : insert x l

 */

/**
  * lex 2.4 without Matryoshka
  */
object CataInsertionSort0204NoMat extends App {

  case class List_cata[X,U](a: U, f:(X,U) => U)

  def list_cata[X,U](lc: List_cata[X,U])(l:MList[X]): U = {
    l match {
      case MNil => lc.a
      case x:MCons[X] => lc.f(x.h, list_cata(lc)(x.t))
    }
  }

  def insert(x:Int, list: MList[Int]): MList[Int] = list match {
    case MNil => MCons(x, MNil)
    case MCons(h,t) => if (x < h) MCons(x, MCons(h, t)) else MCons(h, insert(x,t))
  }

  val insertionSort = list_cata[Int, MList[Int]](List_cata[Int, MList[Int]](MNil, insert)) _

  // 5 2 9 6
  println(insertionSort(MCons(5, MCons(2, MCons(9, MCons(6, MNil))))))  //2 5 6 9

}
