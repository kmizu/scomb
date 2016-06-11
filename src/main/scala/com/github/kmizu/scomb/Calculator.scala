package com.github.kmizu.scomb

object Calculator extends SComb {
  def expression: Parser[Int] = A
  def A: Parser[Int] = M.chainl {
    string("+").map{op => (lhs: Int, rhs: Int) => lhs + rhs} |
    string("-").map{op => (lhs: Int, rhs: Int) => lhs - rhs}
  }
  def M: Parser[Int] = P.chainl {
    string("*").map{op => (lhs: Int, rhs: Int) => lhs * rhs} |
      string("/").map{op => (lhs: Int, rhs: Int) => lhs / rhs}
  }
  def P: Parser[Int] = (for {
    _ <- string("("); e <- expression; _ <- string(")") } yield e) | number
  def number: Parser[Int] = oneOf('0'to'9').*.map{digits => digits.mkString.toInt}

  def main(args: Array[String]): Unit = {
    println(expression("1+2*3"))
    println(expression("1+5*3/4"))
    println(expression("(1+5)*3/2"))
    println(expression("(1-5)*3/2"))
  }
}
