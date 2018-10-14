package com.mhm.matryoshka.hello

import com.mhm.matryoshka.model.{MCons, MList, MNil}


/**
  * hylomorphism done without Matryoshka
  * based on lex, section 2.3, the list hylomorphism
  *
  * lex - "Sorting morphisms", by Lex Augusteijn, Eindhoven
  */


/**

Equivalent Haskell:

  type List_cata x u = (u, x->u->u)

  data Either a b = Left a | Right b

  type List_ana u x = u -> Either () (x,u)

  type List_hylo u x v = (List_ana u x, List_cata x v)

  list_hylo :: List_hylo u x v -> u -> v
  list_hylo (d, (a,f)) = hylo where
    hylo u =  case d u of
              Left _ => a
              Right(x,l) => f x (hylo l)

 */

/**
  * lex 2.3 without Matryoshka
  */
object Hylo0203NoMat extends App {

  def list_hylo[U, X, V](fun: U => Either[Unit, (X,U)], c: (V, (X,V) => V))(u : U): V = {
    fun(u) match {
      case Left(_) => c._1
      case Right((x,l)) => c._2(x, list_hylo(fun, c)(l))
    }
  }

  def destruct_count(n: Int): Either[Unit, (Int, Int)] = n match {
    case 0 => Left(())
    case x => Right(x, x-1)
  }

  val fac = list_hylo[Int, Int, Int](destruct_count, (1, _ * _)) _

  println(fac(5)) // 120

}
