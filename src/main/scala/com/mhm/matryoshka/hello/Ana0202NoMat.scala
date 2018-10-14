package com.mhm.matryoshka.hello

import com.mhm.matryoshka.model.{MCons, MList, MNil}


/**
  * anamorphism done without Matryoshka
  * based on lex, section 2.2, the list anamorphism
  *
  * lex - "Sorting morphisms", by Lex Augusteijn, Eindhoven
  */


/**

Equivalent Haskell:

  data Either a b = Left a | Right b

  type List_ana u x = u -> Either () (x,u)

  list_ana :: List_ana u x -> u -> [x]
  list_ana a = ana where
    ana u = case a u of
            Left _ -> []
            Right (x,l) -> x : ana l

 */

/**
  * lex 2.2 without Matryoshka
  */
object Ana0202NoMat extends App {

  def list_ana[X,U](fun: U => Either[Unit, (X,U)])(u : U): MList[X] = {
    fun(u) match {
      case Left(_) => MNil
      case Right((x,l)) => MCons[X](x, list_ana(fun)(l))
    }
  }

  def destruct_count(n: Int): Either[Unit, (Int, Int)] = n match {
    case 0 => Left(())
    case x => Right(x, x-1)
  }

  val count = list_ana(destruct_count) _

  println(count(5))

}
