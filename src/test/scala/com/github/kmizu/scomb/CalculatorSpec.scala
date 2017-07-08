package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class CalculatorSpec extends FunSpec with DiagrammedAssertions {
  object Calculator extends SCombinator[Int] {
    def root: Parser[Int] = expression
    def expression: Parser[Int] = A
    def A: Parser[Int] = chainl(M) {
      $("+").map{op => (lhs: Int, rhs: Int) => lhs + rhs} |
        $("-").map{op => (lhs: Int, rhs: Int) => lhs - rhs}
    }
    def M: Parser[Int] = chainl(P) {
      $("*").map{op => (lhs: Int, rhs: Int) => lhs * rhs} |
        $("/").map{op => (lhs: Int, rhs: Int) => lhs / rhs}
    }
    def P: Parser[Int] = (for {
      _ <- string("("); e <- expression; _ <- string(")") } yield e) | number
    def number: Parser[Int] = set('0'to'9').*.map{ digits => digits.mkString.toInt}
  }
  import Calculator._

  describe("A Calculator") {
    var input = ""
    it("should parse correct expressions") {
      input = "1+2*3"
      assert(parse(input) == Result.Success(7))
      input = "1+5*3/4"
      assert(parse(input) == Result.Success(4))
      input = "(1+5)*3/2"
      assert(parse(input) == Result.Success(9))
    }
    it("cannot parse incorret expressions") {
      input = "(1-5) *3/2"
      val failure = parse(input).asInstanceOf[Result.Failure]
      assert(Location(1, 6) == failure.location)
      assert("Unconsumed Input: *3/2" == failure.message)
    }
  }
}
