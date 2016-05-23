package com.github.kmizu.scomb

object Calculator extends SComb {
  def expression: Parser[Int] = add
  def add: Parser[Int] = (mult ~ (string("+") ~ mult | string("-") ~ mult).*).map{
    case (l, rs) => rs.foldLeft(l) { case (e, ("+", r)) => e + r; case (e, ("-", r)) => e - r }
  }
  def mult: Parser[Int] = (prm ~ (string("*") ~ prm | string("/") ~ prm).*).map{
    case (l, rs) => rs.foldLeft(l) { case (e, ("*", r)) => e * r; case (e, ("/", r)) => e / r }
  }
  def prm: Parser[Int] = (for {
    _ <- string("("); e <- expression; _ <- string(")") } yield e) | number
  def number: Parser[Int] = oneOf('0'to'9').*.map{digits => digits.mkString.toInt}

  def main(args: Array[String]): Unit = {
    println(expression("1+2*3"))
    println(expression("1+5*3/4"))
    println(expression("(1+5)*3/2"))
  }
}
