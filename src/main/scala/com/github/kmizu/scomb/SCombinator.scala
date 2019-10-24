package com.github.kmizu.scomb

import scala.collection.mutable
import scala.util.matching.Regex

trait SCombinator {self =>
  case class ~[+A, +B](a: A, b: B)

  protected var input: String = ""

  protected var recent: Option[Failure] = None

  protected var fatal: Option[Error] = None

  protected val locations: mutable.Map[Int, Location] = mutable.Map[Int, Location]()

  private[this] val DefaultLabel: String = "fail"

  protected final val DefaultSpace: Parser[String] = (
    $(" ") | $("\t") | $("\b") | $("\f") | $("\r\n") | $("\r") | $("\n")
  )

  protected final def defaultToken(symbol: String): Parser[String] = for {
    s <- $(symbol)
    _ <- DefaultSpace.*
  } yield s

  def EOF: Parser[String] = parserOf{index =>
    if(input.length == index)
      Success("", index)
    else
      Failure(s"expected eof, actual: `${input.charAt(index)}`", index)
  }

  /**
    * A Parser is used to parse the input from the outer class instances.
    * It returns the result of type T as the ParseResult.
    * @tparam T the result type
    */
  abstract class Parser[+T] extends (Int => ParseResult[T]) {
    /**
      * Return the same parser except it replace the failure
      * message with <code>message</code>
      * @param message
      * @return
      */
    def replace(message: String): Parser[T] = parserOf{ index =>
      this(index) match {
        case r@Success(_, _) => r
        case Failure(_, index, label) => Failure(message, index, label)
        case Error(_, index) => Error(message, index)
      }
    }

    /**
      * Parses input from the start index
      * @param index the start index
      * @return the parse result, which type is T
      */
    def apply(index: Int): ParseResult[T]

    /**
      * Returns a Parser that represents repetition (0 or more)
      */
    def * : Parser[List[T]] = parserOf{index =>
      def repeat(index: Int): ParseResult[List[T]] = this(index) match {
        case Success(value, next1) =>
          repeat(next1) match {
            case Success(result, next2) =>
              Success(value::result, next2)
            case r => throw new RuntimeException("cannot be " + r)
          }
        case Failure(message, next, DefaultLabel) =>
          Success(Nil, index)
        case failure@Failure(message, next, label) =>
          failure
        case f@Error(_, _) =>
          f
      }
      repeat(index) match {
        case r@Success(_, _) => r
        case r:ParseNonSuccess => r
      }
    }

    /**
      * Returns a Parser that represents repetition (1 or more).
      * It is same as
      * <pre>
      *   (this ~ this.*).map { case hd ~ tl => hd :: tl }
      * </pre>
      * @return
      */
    def + : Parser[List[T]] = this ~ this.* ^^ { case hd ~ tl =>
      hd :: tl
    }

    /**
      * Returns a sequential Parser consists of <code>this</code> and <code>rhs</code>.
      * Note that the result of <code>this</code> is ignored.
      * It is same as
      * <pre>
      *   for {
      *     _ <- this
      *     u <- rhs
      *   } yield u
      * </pre>
      */
    def >>[U](rhs: Parser[U]): Parser[U] = for {
      _ <- this
      u <- rhs
    } yield u

    /**
      * This method is same as `>>` method.
      * The only difference is operator precedence.
      */
    def ~>[U](rhs: Parser[U]): Parser[U] = this >> rhs

    /**
      * Returns a sequential Parser consists of <code>this</code> and <code>rhs</code>.
      * Note that the result of <code>rhs</code> is ignored.
      * It is same as
      * <pre>
      *   for {
      *     t <- this
      *     _ <- rhs
      *   } yield t
      * </pre>
      */
    def <<[U](rhs: Parser[U]): Parser[T] = for {
      t <- this
      _ <- rhs
    } yield t

    /**
      * This method is same as `<<` method.
      * The only difference is operator precedence.
      */
    def ~<[U](rhs: Parser[U]): Parser[T] = this << rhs

    /**
      * Returns a repetition Parser (0 or more), which separator is
      * <code>separator</code>
      * @param separator this parses separator, which result is ignored
      * @return
      */
    def repeat0By(separator: Parser[Any]): Parser[List[T]] = {
      this.repeat1By(separator).? ^^ {
        case None => Nil
        case Some(list) => list
      }
    }

    /**
      * Returns a repetition Parser (1 or more), which separator is
      * <code>separator</code>
      * @param separator this parses separator, which result is ignored
      * @return
      */
    def repeat1By(separator: Parser[Any]): Parser[List[T]] = {
      this ~ (separator ~ this).* ^^ { case b ~ bs =>
        bs.foldLeft(b::Nil) { case (bs, _ ~ b) =>
          b::bs
        }.reverse
      }
    }

    /**
      * Returns a sequential Parser consists of <code>this</code> and <code>rhs</code>.
      */
    def ~[U](right: Parser[U]) : Parser[T ~ U] = parserOf{index =>
      this(index) match {
        case Success(value1, next1) =>
          right(next1) match {
            case Success(value2, next2) =>
              Success(new ~(value1, value2), next2)
            case failure@Failure(_, _, _) =>
              failure
            case fatal@Error(_, _) =>
              fatal
          }
        case failure@Failure(_, _, _) =>
          failure
        case fatal@Error(_, _) =>
          fatal
      }
    }

    /**
      * Returns a repetition Parser (0 or 1)
      */
    def ? : Parser[Option[T]] = parserOf{index =>
      this(index) match {
        case Success(v, i) => Success(Some(v), i)
        case Failure(message, i, DefaultLabel) => Success(None, index)
        case failure@Failure(_, _, _) => 
          failure
        case fatal@Error(_, _) =>
          fatal
      }
    }

    /**
      * Returns an alternation Parser.
      * It first trys <code>this</code>.
      * It trys <code>rhs</code> iff <code>this</code> failed.
      * @param rhs the alternation
      * @tparam U the result type of <code>rhs</code>
      * @return
      */
    def |[U >: T](rhs: Parser[U]): Parser[U] = parserOf{index =>
      this(index) match {
        case success@Success(_, _) => success
        case Failure(_, _, DefaultLabel) => rhs(index)
        case failure@Failure(_, _, _) => failure
        case fatal@Error(_, _) => fatal
      }
    }

    /**
      * Returns an alternation parser.
      * It first trys <code>this</code>.
      * It trys <cdde>rhs></code> iff <code>this</code> failed and parameters are appropriate catchLabels.
      */
    def |~[U >: T](rhs: Parser[U], catchLabels: String*): Parser[U] = parserOf{index =>
      this(index) match {
        case success@Success(_, _) => success
        case Failure(_, _, label) if catchLabels.contains(label) => rhs(index)
        case failure@Failure(_, _, _) => failure
        case fatal@Error(_, _) => fatal
      }
    }

    /**
      * Returns a alternation parser.
      * It selects longest match.
      */
    def |||[U >: T](rhs: Parser[U]): Parser[U] = parserOf{index =>
      (this(index), rhs(index)) match {
        case (Success(v1, index1), Success(v2, index2)) =>
          if((index1 - index) > (index2 - index))
            Success(v1, index1)
          else
            Success(v2, index2)
        case (s@Success(_, _), _) =>
          s
        case (_, s@Success(_, _)) =>
          s
        case (f@Failure(_, _, _), _) =>
          f
        case (f@Error(_, _), _) =>
          f
      }
    }

    /**
      * Returns a Parser
      * which only succeeds iff <code>predicate(result)</code> is <code>true</code>.
      * @param message the error message in that <code>predicate(result)</code> is <code>false</code>
      */
    def filter(
      predicate: T => Boolean,
      message: String = "not matched to predicate"): Parser[T] = parserOf{index =>
      this(index) match {
        case Success(value, next) =>
          if(predicate(value))
            Success(value, next)
          else
            Failure(message, index)
        case failure@Failure(_, _, _) =>
          failure
        case error@Error(_, _) =>
          error
      }
    }

    def labeled(newLabel: String): Parser[T] = parserOf{index =>
      this(index) match {
        case success@Success(_, _) => success
        case Failure(message, index, oldLabel) =>
          Failure(message, index, newLabel)
        case error@Error(_, _) =>
          error
      }
    }

    def l(newLabel: String): Parser[T] = labeled(newLabel)

    /**
      * Replace the failure parser with the error parser.
      * It is used to suppress backtracks.
      * @param message the error message
      */
    def withErrorMessage(message: String): Parser[T] = parserOf{ index =>
      this(index) match {
        case r@Success(_, _) => r
        case Failure(_, index, _) => Error(message, index)
        case r@Error(_, _) => r
      }
    }

    /**
      * This method is same as `withErrorMessage()` except for	
      * no message being specified.	
      * @return	
      */	
    def commit: Parser[T] = parserOf{index =>	
      this(index) match {	
        case r@Success(_, _) => r	
        case Failure(message, index, _)  => Error(message, index)	
        case r@Error(_, _) => r	
      }	
    }

    /**
      * It is same as <code>filter</code>
      */
    def withFilter(predicate: T => Boolean): Parser[T] = filter(predicate)

    /**
      * Returns a parser that the result is converted from <code>T</code> to <code>U</code>
      * @param function the result converter
      */
    def map[U](function: T => U): Parser[U] = parserOf{index =>
      this(index) match {
        case Success(value, next) => Success(function(value), next)
        case failure@Failure(_, _, _) => failure
        case fatal@Error(_, _) => fatal
      }
    }

    /**
      * It is same as <code>map</code>
      */
    def ^^[U](function: T => U): Parser[U] = map(function)

    /**
      * Returns a parser such that
      * 1. this parses from current position
      * 2. convert the intermediate result of type <code>T</code> to a parser.
      * 3. the parser parses from the next position and returns the result of type <code>U</code>
      *
      * @param function
      * @tparam U
      * @return
      */
    def flatMap[U](function: T => Parser[U]): Parser[U] = parserOf{index =>
      this(index) match {
        case Success(value, next) =>
          function.apply(value).apply(next)
        case failure@Failure(_, _, _) =>
          failure
        case fatal@Error(_, _)=>
          fatal
      }
    }
  }

  /**
    * An alias of Parser[T].
    * @tparam T a type parameter of Parser
    */
  type P[+T] = Parser[T]

  /**
    * An ADT that represent the parse result.
    * @tparam T the type of value
    */
  sealed abstract class ParseResult[+T] {
    def index: Int
    def value: Option[T]
  }

  /**
    * An ADT that represent the parse result in the case that the parser failed.
    */
  sealed abstract class ParseNonSuccess extends ParseResult[Nothing] {
    def message: String
  }

  /**
    * A data constructor in the case parser succeed.
    * @param semanticValue the semantic value
    * @param index the next index
    * @tparam T the type of value
    */
  case class Success[+T](semanticValue: T, override val index: Int) extends ParseResult[T] {
    override def value: Option[T] = Some(semanticValue)
  }

  /**
   * A data constructor in the case parser failed with catch-labels. It can be recovered by <code>|~</code> operator
   */
  case class Failure(override val message: String, override val index: Int, label: String) extends ParseNonSuccess {
    self.recent match {
      case None => self.recent = Some(this)
      case Some(failure) if index >= failure.index => self.recent = Some(this)
      case _ => // Do nothing
    }
    override def value: Option[Nothing] = None
  }
  object Failure {
    def apply(message: String, index: Int): Failure = Failure(message, index, DefaultLabel)
  }

  /**
   * A data constructor in the case parser failed. It must not be recovered.
   * @param message the error message
   * @param index the next index
   */
  case class Error(override val message: String, override val index: Int) extends ParseNonSuccess {
    override def value: Option[Nothing] = None
  }

  protected final def isEOF(index: Int): Boolean = index >= input.length

  protected final def current(index: Int): String = input.substring(index)

  /**
    * Calculate all the lines and columns correspond with indexed.
    * This is called from <code>parse</code> method.
    */
  protected final def calculateLocations(): Unit = {
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

  final def parsePartial[R](rule: Parser[R], input: String): ParseResult[R] = synchronized {
    this.input = input
    this.recent = None
    this.locations.clear()
    calculateLocations()
    rule(0) match {
      case s@Success(_, _) => s
      case f@Failure(_, _, label) =>
        if(label == DefaultLabel) {
          this.recent.get
        } else {
          f
        }
      case f@Error(_, _) =>
        f
    }
  }

  final def parse[R](rule: Parser[R], input: String): Result[R] = synchronized {
    parsePartial(rule, input) match {
      case Success(value, i) =>
        if(isEOF(i)) {
          Result.Success(value)
        } else {
          Result.Failure(locations(i), s"unconsumed input:`${current(i)}`")
        }
      case Failure(message, index, label) =>
        if(label == DefaultLabel) {
          Result.Failure(locations(index), message)
        } else {
          Result.Failure(locations(index), s"${message} in <${label}>")
        }
      case Error(message, index) =>
        Result.Failure(locations(index), s"${message}")
    }
  }

  def % : Parser[Location] = parserOf{index =>
    Success(locations(index), index)
  }

  /**
    * Rerurns a parser that matches any element of <code>seqs</code>
    */
  final def set(seqs: Seq[Char]*): Parser[String] = parserOf{ index =>
    if(isEOF(index) || !seqs.exists(seq => seq.exists(ch => ch == current(index).charAt(0))))
      Failure(s"expected:${seqs.mkString("[", ",", "]")}", index)
    else Success(current(index).substring(0, 1), index + 1)
  }

  /**
    * Returns a parser that matches <code>literal</code>
    * @return
    */
  final def regularExpression(literal: Regex): Parser[String] = parserOf{index =>
    if(isEOF(index)) {
      Failure(s"expected:`${literal}` actual:EOF", index)
    } else {
      val substring = current(index)
      literal.findPrefixOf(substring) match {
        case Some(prefix) => Success(prefix, index + prefix.length)
        case None => Failure(s"expected:`${literal}` actual: `${current(index)(0)}`", index)
      }
    }
  }

  /**
    * Returns a rule parser which argument is a body parser
    */
  final def rule[A](body: => Parser[A]): Parser[A] = parserOf { index =>
    body(index)
  }

  /**
    * It is same as <code>regularExpression</code> method.
    */
  final def r(literal: Regex): Parser[String] = regularExpression(literal)

  final def string(literal: String): Parser[String] = parserOf{index =>
    if(literal.length > 0 && isEOF(index)) {
      Failure(s"expected:`${literal}` actual:EOF", index)
    } else if(current(index).startsWith(literal)) {
      Success(literal, index + literal.length)
    } else {
      Failure(s"expected:`${literal}` actual:`${current(index)(0)}`", index)
    }
  }

  /**
    * Returns a parser that matches any character except EOF.
    */
  final def any: Parser[Char] = parserOf{index =>
    if(isEOF(index)) {
      Failure(s"unexpected EOF", index)
    } else {
      Success(current(index).charAt(0), index + 1)
    }
  }

  /**
    * Returns a parser that succeeds iff <code>parser</code> fails.
    */
  final def not(parser: Parser[Any]): Parser[Any] = parserOf{index =>
    parser(index) match {
      case Success(_, _) => Failure("not expected", index)
      case Failure(_, _, _) => Success("", index)
      case f@Error(_, _) => f
    }
  }

  /**
    * It is same as <code>string</code> method.
    */
  final def $(literal: String): Parser[String] = string(literal)

  /**
    * Returns a parser that matches one character without <code>char</code>
    */
  final def except(char: Char): Parser[String] = parserOf{index =>
    if(isEOF(index)) {
      Failure(s"unexpected EOF", index)
    } else if(current(index).charAt(0) != char) {
      Success("" + current(index).charAt(0), index + 1)
    } else {
      Failure(s"unexpected character:${char}", index)
    }
  }

  final def predict[A](cases: (Char, Parser[A])*): Parser[A] = parserOf{index =>
    def newFailureMessage(head: Char, cases: Map[Char, Parser[A]]): String = {
      val expectation = cases.map{ case (ch, _) => ch}.mkString("[", ",", "]")
      s"expect:${expectation} actual:${head}"
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
