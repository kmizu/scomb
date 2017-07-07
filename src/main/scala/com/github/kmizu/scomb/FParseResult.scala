package com.github.kmizu.scomb

sealed abstract class FParseResult[+T] {
  def value: Option[T]
}
object FParseResult {
  case class Success[+T](semanticValue: T) extends FParseResult[T] {
    override def value: Option[T] = Some(semanticValue)
  }
  case class Failure(location: Location, message: String) extends FParseResult[Nothing] {
    override def value: Option[Nothing] = None
  }
}

