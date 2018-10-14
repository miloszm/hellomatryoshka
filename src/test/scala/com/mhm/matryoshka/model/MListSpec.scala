package com.mhm.matryoshka.model

import org.scalatest.{FlatSpec, Matchers}

class MListSpec extends FlatSpec with Matchers {
  "findMin" should "return min for non empty list" in {
    val lst = MCons(5, MCons(9, MCons(2, MCons(6, MNil))))
    MList.findMin(lst) shouldBe Some(2)
  }
  "findMin" should "return min for empty list" in {
    MList.findMin(MNil) shouldBe None
  }
  "remove" should "return list without a given element for non empty list" in {
    val lst = MCons(5, MCons(9, MCons(2, MCons(6, MNil))))
    MList.remove(6, lst) shouldBe MCons(5, MCons(9, MCons(2, MNil)))
  }
  "remove" should "return same list if empty" in {
    MList.remove(6, MNil) shouldBe MNil
  }
}