package com.mhm.matryoshka.hello


import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import slamdata.Predef._

import scalaz.Functor


/**
  * basic catamorphism example from documentation
  * to see that Matryoshka is included properly
  */


object HelloMatryoshka extends App {


  sealed trait Tree[A]
  final case class Node[A](label: Int, left: A, right: A) extends Tree[A]
  final case class Leaf[A](label: Int)                    extends Tree[A]
  final case class EmptyTree[A]()                         extends Tree[A]


  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Node(label, l, r) => Node(label, f(l), f(r))
      case Leaf(label)       => Leaf[B](label)
      case EmptyTree()       => EmptyTree[B]()
    }
  }

  //     42
  //    /  \
  //   28  83
  //  /  \
  // 12  32
  //    /  \
  //   29  35
  val tree: Fix[Tree] = Fix(Node(
    42,
    Fix(Node(
      28,
      Fix(Leaf(12)),
      Fix(Node(
        32,
        Fix(Leaf(29)),
        Fix(Leaf(35))
      ))
    )),
    Fix(Leaf(83))
  ))


  val printLabels: Algebra[Tree, Unit] = {
    case Node(l, _, _) => println(s"visiting node $l")
    case Leaf(l)       => println(s"visiting leaf $l")
    case EmptyTree()   => println(s"visiting empty")
  }


  tree.cata(printLabels)


  val merge: Algebra[Tree, List[Int]] = {
    case Node(i, l, r) => (l ++ List(i) ++ r) (List.canBuildFrom)
    case Leaf(i)       => List(i)
    case EmptyTree()   => Nil
  }

//  tree.cata(merge)
  println(tree.cata(merge))

}
