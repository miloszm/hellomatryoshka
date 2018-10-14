package com.mhm.matryoshka.hello

import com.mhm.matryoshka.model.MList.{findMin, remove}
import com.mhm.matryoshka.model.{MCons, MList, MNil}


/**
  * lex 2.5.1
  */
object Ana02051SelectionSort extends App {

  def list_ana[X,U](fun: U => Either[Unit, (X,U)])(u : U): MList[X] = {
    fun(u) match {
      case Left(_) => MNil
      case Right((x,l)) => MCons[X](x, list_ana(fun)(l))
    }
  }

  type Extract = MList[Int] => (Int, MList[Int])

  def select(extract: Extract)(l: MList[Int]): Either[Unit, (Int, MList[Int])] = l match {
    case MNil => Left(())
    case list => Right( extract(list) )
  }

  def removeMin(l: MList[Int]): (Int, MList[Int]) = {
    val minOpt = findMin(l)
    minOpt match {
      case Some(m) => (m, remove(m,l))
      case _ => (0, MNil)
    }
  }

  def extract(l: MList[Int]) = removeMin(l)

  val lst = MCons(5, MCons(2, MCons(9, MCons(6, MNil))))

  def selection_sort(l: MList[Int])(extractFun: Extract) = list_ana[Int, MList[Int]](select(extractFun))(l)

  val sorted = selection_sort(lst)(extract)

  println(sorted)

}
