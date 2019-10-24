package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class JsonSpec extends FunSpec with DiagrammedAssertions {
  sealed abstract class JValue
  case class JObject(properties: (String, JValue)*) extends JValue
  case class JArray(elements: JValue*) extends JValue
  case class JString(value: String) extends JValue
  case class JNumber(value: Double) extends JValue
  case class JBoolean(value: Boolean) extends JValue
  case object JNull extends JValue
  object JsonParser extends SCombinator {
    def root: Parser[JValue] = for{
      _ <- DefaultSpace.*
      v <- jvalue
    } yield v

    def escape(ch: Char): Char = ch match {
      case ' ' => ' '
      case 't' => '\t'
      case 'f' => '\f'
      case 'b' => '\b'
      case 'r' => '\r'
      case 'n' => '\n'
      case '\\' => '\\'
      case '"' => '"'
      case '\'' => '\''
      case otherwise => otherwise
    }

    lazy val LBRACKET = defaultToken("[")
    lazy val RBRACKET = defaultToken("]")
    lazy val LBRACE = defaultToken("{")
    lazy val RBRACE = defaultToken("}")
    lazy val COLON = defaultToken(":")
    lazy val COMMA = defaultToken(",")
    lazy val TRUE = defaultToken("true")
    lazy val FALSE = defaultToken("false")
    lazy val NULL = defaultToken("null")

    lazy val jvalue: P[JValue] = rule(jobject | jarray | jstring | jnumber | jboolean | jnull)

    lazy val jobject: P[JValue] = rule{for {
      _ <- LBRACE
      properties <- pair.repeat0By(COMMA)
      _ <- RBRACE.l("RBRACE")
    } yield JObject(properties:_*)}

    lazy val pair: P[(String, JValue)] = rule{for {
      key <- string
      _ <- COLON.l("COLON")
      value <- jvalue
    } yield (key, value)}

    lazy val jarray: P[JValue] = rule{for {
      _ <- LBRACKET
      elements <- jvalue.repeat0By(COMMA)
      _ <- RBRACKET.l("rbracket")
    } yield JArray(elements:_*)}

    lazy val string: Parser[String] = rule{for {
      _ <- $("\"")
      contents <- ($("\\") ~ any ^^ { case _ ~ ch => escape(ch).toString} | except('"')).*
      _ <- $("\"").l("double quote")
      _ <- DefaultSpace.*
    } yield contents.mkString}

    lazy val jstring: Parser[JValue] = rule(string ^^ {v => JString(v)})

    lazy val jnumber: Parser[JValue] = rule{for {
      value <- (set('0'to'9').+) ^^ { case digits => JNumber(digits.mkString.toInt) }
      _ <- DefaultSpace.*
    } yield value}

    lazy val jboolean: Parser[JValue] = rule(
      TRUE ^^ {_ => JBoolean(true)}
    | FALSE ^^ {_ => JBoolean(false)}
    )

    lazy val jnull: Parser[JValue] = rule(NULL ^^ {_ => JNull})

    def parse(input: String): Result[JValue] = parse(root, input)
  }

  import JsonParser._

  describe("JSONParser can parse basic values") {
    it("null") {
      assert(Some(JNull) == parse("null").value)
      assert(Some(JNull) == parse(" null").value)
      assert(Some(JNull) == parse(" null ").value)
      assert(Some(JNull) == parse("null  ").value)
      assert(Some(JNull) == parse("  null").value)
    }
    it("boolean") {
      assert(Some(JBoolean(true)) == parse("true").value)
      assert(Some(JBoolean(false)) == parse("false").value)
      assert(Some(JBoolean(true)) == parse("true ").value)
      assert(Some(JBoolean(true)) == parse(" true").value)
      assert(Some(JBoolean(true)) == parse(" true ").value)
      assert(Some(JBoolean(false)) == parse("false ").value)
      assert(Some(JBoolean(false)) == parse(" false").value)
      assert(Some(JBoolean(false)) == parse(" false ").value)
    }
    it("number") {
      assert(Some(JNumber(0)) == parse("0").value)
      assert(Some(JNumber(0)) == parse(" 0").value)
      assert(Some(JNumber(0)) == parse("0 ").value)
      assert(Some(JNumber(0)) == parse(" 0 ").value)
      assert(Some(JNumber(200)) == parse("200").value)
      assert(Some(JNumber(200)) == parse(" 200").value)
      assert(Some(JNumber(200)) == parse("200 ").value)
      assert(Some(JNumber(200)) == parse(" 200 ").value)
      assert(Some(JNumber(300)) == parse("300").value)
      assert(Some(JNumber(300)) == parse(" 300").value)
      assert(Some(JNumber(300)) == parse("300 ").value)
      assert(Some(JNumber(300)) == parse(" 300 ").value)
    }

    it("string") {
      assert(Some(JString("")) == parse("\"\"").value)
    }
  }

  describe("A JsonParser") {
    it("should parse an object") {
      assert(Some(JObject()) == parse("{}").value)
      assert(Some(JObject("k" -> JObject())) == parse("{\"k\":{}}").value)
      assert(Some(JObject("x" -> JNumber(100), "y" -> JNumber(200))) == parse("{\"x\": 100, \"y\": 200}").value)
    }
    it("should parse an array") {
      assert(Some(JArray()) == parse("[]").value)
      assert(Some(JArray(JArray())) == parse("[[]]").value)
      assert(Some(JArray(JNumber(1), JNumber(2), JNumber(3))) == parse("[1, 2, 3]").value)
      assert(Some(JArray(JObject())) == parse("[{}]").value)
    }
  }

  describe("The JsonParser") {
    it("cannot parse incorrect object") {
      val failure = parse("{").asInstanceOf[Result.Failure]
      assert(Location(1, 2) == failure.location)
      assert("""expected:`}` actual:EOF in <RBRACE>""" == failure.message)
    }
  }

  describe("The JsonParser") {
    it("cannot parse incorrect array") {
      val failure = parse("[1, 2, ]").asInstanceOf[Result.Failure]
      assert(Location(1, 6) == failure.location)
      assert("""expected:`]` actual:`,` in <rbracket>""" == failure.message)
    }
  }
}
