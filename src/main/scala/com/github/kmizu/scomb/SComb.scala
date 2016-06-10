package com.github.kmizu.scomb

abstract class SComb {
  sealed class ParseResult[+T]
  case class ParseSuccess[+T](value: T, next: String) extends ParseResult[T]
  case object ParseFaiure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def oneOf(seqs: Seq[Char]*): Parser[String] = input => {
    if(input.length == 0 || !seqs.exists(seq => seq.exists(ch => ch == input.charAt(0)))) ParseFaiure
    else ParseSuccess(input.substring(0, 1), input.substring(1))
  }

  def string(literal: String): Parser[String] = input => {
    if(input.startsWith(literal)) ParseSuccess(literal, input.substring(literal.length)) else ParseFaiure
  }

  implicit class RichParser[T](val self: Parser[T]) {
    def * : Parser[List[T]] = input => {
      def repeat(input: String): (List[T], String) = self(input) match {
        case ParseSuccess(value, next1) =>
          val (result, next2) = repeat(next1)
          (value::result, next2)
        case ParseFaiure =>
          (Nil, input)
      }
      val (result, next) = repeat(input)
      ParseSuccess(result, next)
    }

    def ~[U](right: Parser[U]) : Parser[(T, U)] = input => {
      self(input) match {
        case ParseSuccess(value1, next1) =>
          right(next1) match {
            case ParseSuccess(value2, next2) =>
              ParseSuccess((value1, value2), next2)
            case ParseFaiure =>
              ParseFaiure
          }
        case ParseFaiure =>
          ParseFaiure
      }
    }

    def chainl(q: Parser[(T, T) => T]): Parser[T] = {
      (self ~ (q ~ self).*).map { case (x, xs) =>
          xs.foldLeft(x) { case (a, (f, b)) =>
              f(a, b)
          }
      }
    }

    def |(right: Parser[T]): Parser[T] = input => {
      self(input) match {
        case success@ParseSuccess(_, _) => success
        case ParseFaiure => right(input)
      }
    }

    def map[U](function: T => U): Parser[U] = input => {
      self(input) match {
        case ParseSuccess(value, next) => ParseSuccess(function(value), next)
        case ParseFaiure => ParseFaiure
      }
    }

    def flatMap[U](function: T => Parser[U]): Parser[U] = input => {
      self(input) match {
        case ParseSuccess(value, next) =>
          function(value)(next)
        case ParseFaiure => ParseFaiure
      }
    }
  }
}
