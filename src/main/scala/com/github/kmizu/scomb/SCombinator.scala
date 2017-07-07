package com.github.kmizu.scomb

import scala.collection.mutable
import scala.util.matching.Regex

abstract class SCombinator[R] {self =>
  case class ~[+A, +B](a: A, b: B)

  protected var input: String = ""

  protected var recent: Option[Failure] = None

  protected val locations: mutable.Map[Int, Location] = mutable.Map[Int, Location]()

  lazy val space: Parser[String] = (
    $(" ") | $("\t") | $("\b") | $("\f") | $("\r\n") | $("\r") | $("\n")
  )

  def token(symbol: String): Parser[String] = for {
    s <- $(symbol)
    _ <- space.*
  } yield s

  sealed abstract class ParseResult[+T] {
    def index: Int
    def value: Option[T]
  }

  sealed abstract class ParseFailure extends ParseResult[Nothing] {
    def message: String
  }

  case class Success[+T](semanticValue: T, override val index: Int) extends ParseResult[T] {
    override def value: Option[T] = Some(semanticValue)
  }

  case class Failure(override val message: String, override val index: Int) extends ParseFailure {
    self.recent match {
      case None => self.recent = Some(this)
      case Some(failure) if index >= failure.index => self.recent = Some(this)
      case _ => // Do nothing
    }
    override def value: Option[Nothing] = None
  }

  case class Fatal(override val message: String, override val index: Int) extends ParseFailure {
    override def value: Option[Nothing] = None
  }

  def root: Parser[R]

  final def isEOF(index: Int): Boolean = index >= input.length

  final def current(index: Int): String = input.substring(index)

  final def calculateLocations(): Unit = {
    var i: Int = 0
    var line: Int = 1
    var column: Int = 1
    val chars = input.toCharArray
    while(i < chars.length) {
      val ch = chars(i)
      ch match {
        case '\n' =>
          locations(i) = Location(line, column)
          line += 1
          column = 1
          i += 1
        case '\r' =>
          if(i == chars.length - 1) {
            locations(i) = Location(line, column)
            line += 1
            column = 1
            i += 1
          } else {
            locations(i) = Location(line, column)
            if(chars(i + 1) == '\n') {
              locations(i + 1) = Location(line, column + 1)
              line += 1
              column = 1
              i += 2
            } else {
              line += 1
              column = 1
              i += 1
            }
          }
        case _ =>
          locations(i) = Location(line, column)
          column += 1
          i += 1
      }
    }
    locations(i) = Location(line, column)
  }

  final def parse(input: String): ParseResult[R] = synchronized {
    this.input = input
    this.recent = None
    this.locations.clear()
    calculateLocations()
    root(0) match {
      case s@Success(_, _) => s
      case f@Failure(_, _) => recent.get
      case f@Fatal(_,  _) => f
    }
  }

  final def parseAll(input: String): Result[R] = synchronized {
    parse(input) match {
      case Success(value, i) =>
        if(isEOF(i)) {
          Result.Success(value)
        } else {
          Result.Failure(locations(i), "Unconsumed Input:" + current(i))
        }
      case Failure(message, index) =>
        Result.Failure(locations(index), message)
      case Fatal(message, index) =>
        Result.Failure(locations(index), message)
    }
  }

  def % : Parser[Location] = parserOf{index =>
    Success(locations(index), index)
  }

  final def oneOf(seqs: Seq[Char]*): Parser[String] = parserOf{index =>
    if(isEOF(index) || !seqs.exists(seq => seq.exists(ch => ch == current(index).charAt(0))))
      Failure(s"Expected:${seqs.mkString("[", ",", "]")}", index)
    else Success(current(index).substring(0, 1), index + 1)
  }

  final def regularExpression(literal: Regex): Parser[String] = parserOf{index =>
    if(isEOF(index)) {
      Failure(s"""Expected:"${literal}" Actual:EOF""", index)
    } else {
      val substring = current(index)
      literal.findPrefixOf(substring) match {
        case Some(prefix) => Success(substring.substring(prefix.length), index + prefix.length)
        case None => Failure(s"""Expected:"${literal}"""", index)
      }
    }
  }

  /**
    * It is just shorthand of `regularExpression` method.
    * @param literal
    * @return
    */
  final def r(literal: Regex): Parser[String] = regularExpression(literal)

  final def string(literal: String): Parser[String] = parserOf{index =>
    if(isEOF(index)) {
      Failure(s"""Expected:"${literal}" Actual:EOF""", index)
    } else if(current(index).startsWith(literal)) {
      Success(literal, index + literal.length)
    } else {
      Failure(s"""Expected:"${literal}"""", index)
    }
  }

  final def any: Parser[Char] = parserOf{index =>
    if(isEOF(index)) {
      Failure(s"Unexpected EOF", index)
    } else {
      Success(current(index).charAt(0), index + 1)
    }
  }

  final def not(parser: Parser[Any]): Parser[Any] = parserOf{index =>
    parser(index) match {
      case Success(_, index) => Failure("Not Expected", index)
      case Failure(_, index) => Success("", index)
      case f@Fatal(_, _) => f
    }
  }

  final def $(literal: String): Parser[String] = string(literal)

  final def except(char: Char): Parser[String] = parserOf{index =>
    if(isEOF(index)) {
      Failure(s"Unexpected EOF", index)
    } else if(current(index).charAt(0) != char) {
      Success("" + current(index).charAt(0), index + 1)
    } else {
      Failure(s"Unexpected Character:${char}", index)
    }
  }

  final def predict[A](cases: (Char, Parser[A])*): Parser[A] = parserOf{index =>
    def newFailureMessage(head: Char, cases: Map[Char, Parser[A]]): String = {
      val expectation = cases.map{ case (ch, _) => ch}.mkString("[", ",", "]")
      s"Expect:${expectation} Actual:${head}"
    }

    if(isEOF(index)) {
      val map: Map[Char, Parser[A]] = Map(cases :_*)
      Failure(newFailureMessage(0, map), index)
    } else {
      val head = current(index).charAt(0)
      val map = Map(cases:_*)
      map.get(head) match {
        case Some(clause) => clause(index)
        case None =>
          Failure(newFailureMessage(head, map), index)
      }
    }
  }

  abstract class Parser[+T] extends (Int => ParseResult[T]) {
    def apply(index: Int): ParseResult[T]

    def * : Parser[List[T]] = parserOf{index =>
      def repeat(index: Int): (List[T], Int) = this(index) match {
        case Success(value, next1) =>
          val (result, next2) = repeat(next1)
          (value::result, next2)
        case Failure(message, next) =>
          (Nil, next)
        case Fatal(_, _) =>
          (Nil, -1)
      }
      val (result, next) = repeat(index)
      if(next >= 0) {
        Success(result, next)
      } else {
        Fatal("fatal error", next)
      }
    }

    def + : Parser[List[T]] = this ~ this.* ^^ { case hd ~ tl =>
      hd :: tl
    }

    def repeat1By(separator: Parser[Any]): Parser[List[T]] = {
      this ~ (separator ~ this).* ^^ { case b ~ bs =>
          bs.foldLeft(b::Nil) { case (bs, _ ~ b) =>
              b::bs
          }.reverse
      }
    }

    def repeat0By(separator: Parser[Any]): Parser[List[T]] = {
      this.repeat1By(separator).? ^^ {
        case None => Nil
        case Some(list) => list
      }
    }

    def ~[U](right: Parser[U]) : Parser[T ~ U] = parserOf{index =>
      this(index) match {
        case Success(value1, next1) =>
          right(next1) match {
            case Success(value2, next2) =>
              Success(new ~(value1, value2), next2)
            case failure@Failure(_, _) =>
              failure
            case fatal@Fatal(_, _) =>
              fatal
          }
        case failure@Failure(message, next) =>
          failure
        case fatal@Fatal(_, _) =>
          fatal
      }
    }

    def ? : Parser[Option[T]] = parserOf{index =>
      this(index) match {
        case Success(v, i) => Success(Some(v), i)
        case Failure(message, i) => Success(None, i)
        case fatal@Fatal(_, _) => fatal
      }
    }

    def |[U >: T](right: Parser[U]): Parser[U] = parserOf{index =>
      this(index) match {
        case success@Success(_, _) => success
        case Failure(_, _) => right(index)
        case fatal@Fatal(_, _) => fatal
      }
    }

    def filter(predicate: T => Boolean): Parser[T] = parserOf{index =>
      this(index) match {
        case Success(value, next) =>
          if(predicate(value))
            Success(value, next)
          else
            Failure("Not matched to predicate", index)
        case failure@Failure(_, _) =>
          failure
        case fatal@Fatal(_, _) =>
          fatal
      }
    }

    def withFilter(predicate: T => Boolean): Parser[T] = filter(predicate)

    def map[U](function: T => U): Parser[U] = parserOf{index =>
      this(index) match {
        case Success(value, next) => Success(function(value), next)
        case failure@Failure(_, _) => failure
        case fatal@Fatal(_, _) => fatal
      }
    }

    def ^^[U](function: T => U): Parser[U] = map(function)

    def flatMap[U](function: T => Parser[U]): Parser[U] = parserOf{index =>
      this(index) match {
        case Success(value, next) =>
          function.apply(value).apply(next)
        case failure@Failure(_, _) =>
          failure
        case fatal@Fatal(_, _) =>
          fatal
      }
    }
  }

  def chainl[T](p: Parser[T])(q: Parser[(T, T) => T]): Parser[T] = {
    (p ~ (q ~ p).*).map { case x ~ xs =>
      xs.foldLeft(x) { case (a, f ~ b) =>
        f(a, b)
      }
    }
  }

  def parserOf[T](function: Int => ParseResult[T]): Parser[T] = new Parser[T] {
    override def apply(index: Int): ParseResult[T] = function(index)
  }
}
