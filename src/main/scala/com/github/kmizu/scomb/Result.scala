package com.github.kmizu.scomb

sealed abstract class Result[+T] {
  def value: Option[T]
}
object Result {
  case class Success[+T](semanticValue: T) extends Result[T] {
    override def value: Option[T] = Some(semanticValue)
  }
  case class Failure(location: Location, message: String) extends Result[Nothing] {
    override def value: Option[Nothing] = None
  }
}

