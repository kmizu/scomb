package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class PrimitiveSpec extends FunSpec with DiagrammedAssertions {
  object P1 extends SCombinator[String] {
    override def root: P[String] = $("")
  }
  object P2 extends SCombinator[String] {
    override def root: P[String] = $("H")
  }
  object P3 extends SCombinator[String] {
    override def root: P[String] = r("""[0-9]+""".r)
  }
  object P4 extends SCombinator[String] {
    override def root: P[String] = any.map{_.toString}
  }
  object P5 extends SCombinator[String] {
    override def root: P[String] = except('H')
  }
  describe("$ combinator") {
    it("""$("") and string("") always succeed""") {
      P1.parsePartial("") match {
        case P1.Success(v, index) =>
          assert("" == v)
          assert(0 == index)
        case _ =>
          assert(false)
      }
    }
    it("""$("H") succeed for string starts with 'H'""") {
      P2.parsePartial("H") match {
        case P2.Success(v, index) =>
          assert("H" == v)
          assert(1 == index)
        case _ =>
          assert(false)
      }
      P2.parsePartial("Hello") match {
        case P2.Success(v, index) =>
          assert("H" == v)
          assert(1 == index)
        case _ =>
          assert(false)
      }
      P2.parsePartial("I") match {
        case P2.Success(_, _) =>
          assert(false)
        case n:P2.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
    }
    it("""r(...) succeed for given regular expression""") {
      P3.parsePartial("") match {
        case P3.Success(_, _) =>
          assert(false)
        case n:P3.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
      P3.parsePartial("012345") match {
        case P3.Success(v, i) =>
          assert("012345" == v)
          assert(6 == i)
        case _:P3.ParseNonSuccess =>
          assert(false)
      }
      P3.parsePartial("012abc") match {
        case P3.Success(v, i) =>
          assert("012" == v)
          assert(3 == i)
        case _:P3.ParseNonSuccess =>
          assert(false)
      }
    }
    it("`any` succeed for any one character") {
      P4.parsePartial("") match {
        case P4.Success(_, _) =>
          assert(false)
        case n:P4.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
      P4.parsePartial("a") match {
        case P4.Success(v, i) =>
          assert("a" == v)
          assert(1 == i)
        case _:P4.ParseNonSuccess =>
          assert(false)
      }
      P4.parsePartial("b") match {
        case P4.Success(v, i) =>
          assert("b" == v)
          assert(1 == i)
        case _:P4.ParseNonSuccess =>
          assert(false)
      }
      P4.parsePartial("ab") match {
        case P4.Success(v, i) =>
          assert("a" == v)
          assert(1 == i)
        case _:P4.ParseNonSuccess =>
          assert(false)
      }
      P4.parsePartial("ba") match {
        case P4.Success(v, i) =>
          assert("b" == v)
          assert(1 == i)
        case _:P4.ParseNonSuccess =>
          assert(false)
      }
    }
    it("""forall c. except(c) succeed iff input doesn't start with c""") {
      P5.parsePartial("") match {
        case P5.Success(_, _) =>
          assert(false)
        case n:P5.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
      P5.parsePartial("H")  match {
        case P5.Success(_, _) =>
          assert(false)
        case n:P5.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
      P5.parsePartial("I") match {
        case P5.Success(v, i) =>
          assert("I" == v)
          assert(1 == i)
        case _:P5.ParseNonSuccess =>
          assert(false)
      }
    }
  }
}
