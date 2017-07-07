package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class RegularExpressionSpec extends FunSpec with DiagrammedAssertions {
  sealed abstract class Regexp
  case class Sequence(lhs: Regexp, rhs: Regexp) extends Regexp
  case class Choice(lhs: Regexp, rhs: Regexp) extends Regexp
  case class Repeat(body: Regexp) extends Regexp
  case class Value(value: Char) extends Regexp

  object RegularExpressionParser extends SCombinator[Regexp] {
    def root: Parser[Regexp] = expression
    def expression: Parser[Regexp] = alternative
    def alternative: Parser[Regexp] = chainl(sequencable) {
      $("|").map{op => (lhs: Regexp, rhs: Regexp) => Choice(lhs, rhs) }
    }
    def sequencable: Parser[Regexp] = chainl(repeatable) {
      $("").map{op => (lhs: Regexp, rhs: Regexp) => Sequence(lhs, rhs) }
    }
    def repeatable: Parser[Regexp] = {
      primary ~ $("*").? ^^ {
        case e ~ Some(_) => Repeat(e)
        case e ~ None => e
      }
    }
    def primary: Parser[Regexp] = (for {
      _ <- string("("); e <- expression; _ <- string(")") } yield e) | character
    def character: Parser[Regexp] = (for {
      _ <- not(oneOf(Seq('\\', '|', '(', ')', '*')))
      ch <- any
    } yield Value(ch)) | (for {
      _ <- $("\\")
      ch <- any
    } yield Value(ch))
  }
  import RegularExpressionParser._

  describe("Parsing regular expressions") {
    var input = ""
    it("succeeds") {
      input = "0"
      assert(parseAll(input) == Result.Success(Value('0')))
      input = "1"
      assert(parseAll(input) == Result.Success(Value('1')))
      input = "9"
      assert(parseAll(input) == Result.Success(Value('9')))
      input = "19"
      assert(parseAll(input) == Result.Success(Sequence(Value('1'), Value('9'))))
      input = "1|9"
      assert(parseAll(input) == Result.Success(Choice(Value('1'), Value('9'))))
      input = "(1|9)0"
      assert(parseAll(input) == Result.Success(Sequence(Choice(Value('1'), Value('9')), Value('0'))))
      input = "(1|9)*"
      assert(parseAll(input) == Result.Success(Repeat(Choice(Value('1'), Value('9')))))
      input = "*"
      assert(parseAll(input) == Result.Failure(Location(1, 1), "Expected:\"\\\""))
    }
  }
}
