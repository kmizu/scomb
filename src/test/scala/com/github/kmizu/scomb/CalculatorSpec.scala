package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class CalculatorSpec extends FunSpec with DiagrammedAssertions {
  object Calculator extends SCombinator {
    def root: Parser[Int] = expression

    def expression: Parser[Int] = rule(A)

    def A: Parser[Int] = rule(chainl(M) {
      $("+").map { op => (lhs: Int, rhs: Int) => lhs + rhs } |
        $("-").map { op => (lhs: Int, rhs: Int) => lhs - rhs }
    })

    def M: Parser[Int] = rule(chainl(P) {
      $("*").map { op => (lhs: Int, rhs: Int) => lhs * rhs } |
      $("/").map { op => (lhs: Int, rhs: Int) => lhs / rhs }
    })

    def P: P[Int] = rule{
      (for {
        _ <- string("("); e <- expression; _ <- string(")")} yield e) | number
    }
    def number: P[Int] = rule(set('0'to'9').+.map{ digits => digits.mkString.toInt})

    def parse(input: String): Result[Int] = parse(root, input)
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
    it("cannot parse incorrect expressions, which ends with unexpected EOF") {
      input = "1+ "
      val failure = parse(input).asInstanceOf[Result.Failure]
      assert(Location(1, 2) == failure.location)
      assert("unconsumed input:`+ `" == failure.message)
    }

    it("cannot parse incorrect expressions, which contains spaces") {
      input = "(1-5) *3/2"
      val failure = parse(input).asInstanceOf[Result.Failure]
      assert(Location(1, 6) == failure.location)
      assert("unconsumed input:` *3/2`" == failure.message)
    }
  }
}
