package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class CalculatorSpec extends FunSpec with DiagrammedAssertions {
  object Calculator extends SCombinator[Int] {
    def root: Parser[Int] = expression
    def expression: Parser[Int] = A
    def A: Parser[Int] = M.chainl {
      $("+").map{op => (lhs: Int, rhs: Int) => lhs + rhs} |
        $("-").map{op => (lhs: Int, rhs: Int) => lhs - rhs}
    }
    def M: Parser[Int] = P.chainl {
      $("*").map{op => (lhs: Int, rhs: Int) => lhs * rhs} |
        $("/").map{op => (lhs: Int, rhs: Int) => lhs / rhs}
    }
    def P: Parser[Int] = (for {
      _ <- string("("); e <- expression; _ <- string(")") } yield e) | number
    def number: Parser[Int] = oneOf('0'to'9').*.map{digits => digits.mkString.toInt}
  }
  import Calculator._

  describe("Parsing mathematical expressions") {
    var input = ""
    it("succeeds") {
      input = "1+2*3"
      assert(parseAll(input) == Success(7, input.length))
      input = "1+5*3/4"
      assert(parseAll(input) == Success(4, input.length))
      input = "(1+5)*3/2"
      assert(parseAll(input) == Success(9, input.length))
    }
    it("fails") {
      input = "(1-5) *3/2"
      assert(parseAll(input).isInstanceOf[Failure])
    }
  }
}
