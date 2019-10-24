package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class PrimitiveSpec extends FunSpec with DiagrammedAssertions {
  object P1 extends SCombinator {
    def root: P[String] = $("")
  }
  object P2 extends SCombinator {
    def root: P[String] = $("H")
  }
  object P3 extends SCombinator {
    def root: P[String] = r("""[0-9]+""".r)
  }
  object P4 extends SCombinator {
    def root: P[String] = any.map{_.toString}
  }
  object P5 extends SCombinator {
    def root: P[String] = except('H')
  }
  describe("$ combinator") {
    it("""$("") and string("") always succeed""") {
      P1.parsePartial(P1.root, "") match {
        case P1.Success(v, index) =>
          assert("" == v)
          assert(0 == index)
        case _ =>
          assert(false)
      }
    }
    it("""$("H") succeed for string starts with 'H'""") {
      P2.parsePartial(P2.root, "H") match {
        case P2.Success(v, index) =>
          assert("H" == v)
          assert(1 == index)
        case _ =>
          assert(false)
      }
      P2.parsePartial(P2.root, "Hello") match {
        case P2.Success(v, index) =>
          assert("H" == v)
          assert(1 == index)
        case _ =>
          assert(false)
      }
      P2.parsePartial(P2.root, "I") match {
        case P2.Success(_, _) =>
          assert(false)
        case n:P2.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
    }
    it("""r(...) succeed for given regular expression""") {
      import P3._
      parsePartial(root, "") match {
        case P3.Success(_, _) =>
          assert(false)
        case n:P3.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
      parsePartial(root, "012345") match {
        case P3.Success(v, i) =>
          assert("012345" == v)
          assert(6 == i)
        case _:P3.ParseNonSuccess =>
          assert(false)
      }
      parsePartial(root, "012abc") match {
        case P3.Success(v, i) =>
          assert("012" == v)
          assert(3 == i)
        case _:P3.ParseNonSuccess =>
          assert(false)
      }
    }
    it("`any` succeed for any one character") {
      import P4._
      parsePartial(root, "") match {
        case P4.Success(_, _) =>
          assert(false)
        case n:P4.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
      parsePartial(root, "a") match {
        case P4.Success(v, i) =>
          assert("a" == v)
          assert(1 == i)
        case _:P4.ParseNonSuccess =>
          assert(false)
      }
      parsePartial(root, "b") match {
        case P4.Success(v, i) =>
          assert("b" == v)
          assert(1 == i)
        case _:P4.ParseNonSuccess =>
          assert(false)
      }
      parsePartial(root, "ab") match {
        case P4.Success(v, i) =>
          assert("a" == v)
          assert(1 == i)
        case _:P4.ParseNonSuccess =>
          assert(false)
      }
      parsePartial(root, "ba") match {
        case P4.Success(v, i) =>
          assert("b" == v)
          assert(1 == i)
        case _:P4.ParseNonSuccess =>
          assert(false)
      }
    }
    it("""forall c. except(c) succeed iff input doesn't start with c""") {
      import P5._
      parsePartial(root, "") match {
        case P5.Success(_, _) =>
          assert(false)
        case n:P5.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
      parsePartial(root, "H")  match {
        case P5.Success(_, _) =>
          assert(false)
        case n:P5.ParseNonSuccess =>
          assert(0 == n.index)
          assert(None == n.value)
      }
      parsePartial(root, "I") match {
        case P5.Success(v, i) =>
          assert("I" == v)
          assert(1 == i)
        case _:P5.ParseNonSuccess =>
          assert(false)
      }
    }
  }
}
