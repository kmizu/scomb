package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class RegularExpressionSpec extends FunSpec with DiagrammedAssertions {
  sealed abstract class Regexp
  case class Sequence(lhs: Regexp, rhs: Regexp) extends Regexp
  case class Choice(lhs: Regexp, rhs: Regexp) extends Regexp
  case class Repeat(body: Regexp) extends Regexp
  case class Value(value: Char) extends Regexp

  object RegularExpressionParser extends SCombinator {
    def root: Parser[Regexp] = expression

    def expression: Parser[Regexp] = alternative

    def alternative: Parser[Regexp] = rule(chainl(sequencable) {
      $("|").map { op => (lhs: Regexp, rhs: Regexp) => Choice(lhs, rhs) }
    })

    def sequencable: Parser[Regexp] = rule(chainl(repeatable) {
      $("").map { op => (lhs: Regexp, rhs: Regexp) => Sequence(lhs, rhs) }
    })

    def repeatable: Parser[Regexp] = rule {
      primary ~ $("*").? ^^ {
        case e ~ Some(_) => Repeat(e)
        case e ~ None => e
      }
    }

    def primary: Parser[Regexp] = rule {
      (for {
        _ <- string("("); e <- expression; _ <- string(")")
      } yield e) | character
    }
    def character: Parser[Regexp] = rule((for {
      _ <- not(set(Seq('\\', '|', '(', ')', '*')))
      ch <- any
    } yield Value(ch)) | (for {
      _ <- $("\\")
      ch <- any
    } yield Value(ch)))
    def parse(input: String): Result[Regexp] = parse(root, input)
  }
  import RegularExpressionParser._

  describe("A regular expression parser") {
    var input = ""
    it("should parse one character") {
      input = "0"
      assert(parse(input) == Result.Success(Value('0')))
      input = "1"
      assert(parse(input) == Result.Success(Value('1')))
      input = "9"
      assert(parse(input) == Result.Success(Value('9')))
    }
    it("should parser multiple characters") {
      input = "19"
      assert(parse(input) == Result.Success(Sequence(Value('1'), Value('9'))))
      input = "ab"
      assert(parse(input) == Result.Success(Sequence(Value('a'), Value('b'))))
      input = "abc"
      assert(parse(input) == Result.Success(Sequence(Sequence(Value('a'), Value('b')), Value('c'))))
    }
    it("should parse more complex expressions") {
      input = "1|9"
      assert(parse(input) == Result.Success(Choice(Value('1'), Value('9'))))
      input = "(1|9)0"
      assert(parse(input) == Result.Success(Sequence(Choice(Value('1'), Value('9')), Value('0'))))
      input = "(1|9)*"
      assert(parse(input) == Result.Success(Repeat(Choice(Value('1'), Value('9')))))
      input = "*"
      assert(parse(input) == Result.Failure(Location(1, 1), "expected:`\\` actual:`*`"))
    }
  }
}
