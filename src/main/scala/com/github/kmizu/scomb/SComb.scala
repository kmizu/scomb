package com.github.kmizu.scomb

abstract class SComb[R] {self =>
  case class ~[A, B](a: A, b: B)
  val input: String
  def root: Parser[R]
  def isEOF(index: Int): Boolean = index >= input.length
  def current(index: Int): String = input.substring(index)
  var recent: Option[ParseResult.Failure] = None

  def parse: ParseResult[R] = root(0) match {
    case s@ParseResult.Success(_, _) => s
    case f@ParseResult.Failure(_, _) => recent.get
    case f@ParseResult.Fatal( _) => f
  }

  def parseAll: ParseResult[R] = parse match {
    case s@ParseResult.Success(_, i) =>
      if(isEOF(i)) s else ParseResult.Failure("input remains: " + current(i), i)
    case otherwise => otherwise
  }

  sealed abstract class ParseResult[+T] {
    def index: Int
  }
  object ParseResult {
    case class Success[+T](value: T, override val index: Int) extends ParseResult[T]
    case class Failure(message: String, override val index: Int) extends ParseResult[Nothing] {
      self.recent match {
        case None => self.recent = Some(this)
        case Some(failure) if index > failure.index => self.recent = Some(this)
        case _ => // Do nothing
      }
    }
    case class Fatal(override val index: Int) extends ParseResult[Nothing]
  }

  type Parser[+T] = Int => ParseResult[T]

  def oneOf(seqs: Seq[Char]*): Parser[String] = index => {
    if(isEOF(index) || !seqs.exists(seq => seq.exists(ch => ch == current(index).charAt(0))))
      ParseResult.Failure(s"expected: ${seqs.mkString("[", ",", "]")}", index)
    else ParseResult.Success(current(index).substring(0, 1), index + 1)
  }

  def string(literal: String): Parser[String] = index => {
    if(isEOF(index)) {
      ParseResult.Failure(s"expected: ${literal} actual: EOF", index)
    } else if(current(index).startsWith(literal)) {
      ParseResult.Success(literal, index + literal.length)
    } else {
      ParseResult.Failure(s"expected: ${literal} actual: ${current(index).substring(0, literal.length)}", index)
    }
  }

  def predict[A](cases: (Char, Parser[A])*): Parser[A] = index => {
    def newFailureMessage(head: Char, cases: Map[Char, Parser[A]]): String = {
      val expectation = cases.map{ case (ch, _) => ch}.mkString("[", ",", "]")
      s"expect: ${expectation} actual: ${head}"
    }

    if(isEOF(index)) {
      val map: Map[Char, Parser[A]] = Map(cases :_*)
      ParseResult.Failure(newFailureMessage(0, map), index)
    } else {
      val head = current(index).charAt(0)
      val map = Map(cases:_*)
      map.get(head) match {
        case Some(clause) => clause(index)
        case None =>
          ParseResult.Failure(newFailureMessage(head, map), index)
      }
    }
  }

  implicit class RichParser[T](val self: Parser[T]) {
    def * : Parser[List[T]] = index => {
      def repeat(index: Int): (List[T], Int) = self(index) match {
        case ParseResult.Success(value, next1) =>
          val (result, next2) = repeat(next1)
          (value::result, next2)
        case ParseResult.Failure(message, next) =>
          (Nil, next)
        case ParseResult.Fatal(_) =>
          (Nil, -1)
      }
      val (result, next) = repeat(index)
      if(next >= 0) {
        ParseResult.Success(result, next)
      } else {
        ParseResult.Fatal(next)
      }
    }

    def repeat1By(separator: Parser[Any]): Parser[List[T]] = {
      self ~ (separator ~ self).* ^^ { case b ~ bs =>
          bs.foldLeft(b::Nil) { case (bs, _ ~ b) =>
              b::bs
          }.reverse
      }
    }

    def repeat0By(separator: Parser[Any]): Parser[List[T]] = {
      self.repeat1By(separator).? ^^ {
        case None => Nil
        case Some(list) => list
      }
    }

    def ~[U](right: Parser[U]) : Parser[T ~ U] = index => {
      self(index) match {
        case ParseResult.Success(value1, next1) =>
          right(next1) match {
            case ParseResult.Success(value2, next2) =>
              ParseResult.Success(new ~(value1, value2), next2)
            case failure@ParseResult.Failure(_, _) =>
              failure
            case fatal@ParseResult.Fatal(_) =>
              fatal
          }
        case failure@ParseResult.Failure(message, next) =>
          failure
        case fatal@ParseResult.Fatal(_) =>
          fatal
      }
    }

    def ? : Parser[Option[T]] = index => {
      self(index) match {
        case ParseResult.Success(v, i) => ParseResult.Success(Some(v), i)
        case ParseResult.Failure(message, i) => ParseResult.Success(None, i)
        case fatal@ParseResult.Fatal(_) => fatal
      }
    }

    def chainl(q: Parser[(T, T) => T]): Parser[T] = {
      (self ~ (q ~ self).*).map { case x ~ xs =>
          xs.foldLeft(x) { case (a, f ~ b) =>
              f(a, b)
          }
      }
    }

    def |(right: Parser[T]): Parser[T] = index => {
      self(index) match {
        case success@ParseResult.Success(_, _) => success
        case ParseResult.Failure(_, _) => right(index)
        case fatal@ParseResult.Fatal(_) => fatal
      }
    }

    def filter(predicate: T => Boolean): Parser[T] = index => {
      self(index) match {
        case ParseResult.Success(value, next) =>
          if(predicate(value))
            ParseResult.Success(value, next)
          else
            ParseResult.Failure("not matched to predicate", index)
        case failure@ParseResult.Failure(_, _) =>
          failure
        case fatal@ParseResult.Fatal(_) =>
          fatal
      }
    }

    def withFilter(predicate: T => Boolean): Parser[T] = filter(predicate)

    def map[U](function: T => U): Parser[U] = index => {
      self(index) match {
        case ParseResult.Success(value, next) => ParseResult.Success(function(value), next)
        case failure@ParseResult.Failure(_, _) => failure
        case fatal@ParseResult.Fatal(_) => fatal
      }
    }

    def ^^[U](function: T => U): Parser[U] = map(function)

    def flatMap[U](function: T => Parser[U]): Parser[U] = index => {
      self(index) match {
        case ParseResult.Success(value, next) =>
          function(value)(next)
        case failure@ParseResult.Failure(_, _) =>
          failure
        case fatal@ParseResult.Fatal(_) =>
          fatal
      }
    }
  }
}
