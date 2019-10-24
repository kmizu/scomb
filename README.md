## SComb: Simple but Powerful Parser Combinator Library in Scala
 
[![Build Status](https://travis-ci.org/kmizu/scomb.png?branch=master)](https://travis-ci.org/kmizu/scomb)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.kmizu/scomb_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.kmizu/scomb_2.13)
[![Scaladoc](http://javadoc-badge.appspot.com/com.github.kmizu/scomb_2.13.svg?label=scaladoc)](http://javadoc-badge.appspot.com/com.github.kmizu/scomb_2.13/com/github/kmizu/scomb/index.html)
[![Reference Status](https://www.versioneye.com/java/com.github.kmizu:scomb_2.13/reference_badge.svg?style=flat)](https://www.versioneye.com/java/com.github.kmizu:scomb_2.13/references)

SComb (su-ko-n-bu) is a simple but powerful parser combinator library written in Scala.
SComb aims to replace [scala-parser-combinators](https://github.com/scala/scala-parser-combinators).

## Features of SComb (0.8.1):

- primitive combinators
  - string literal ( `""` )
  - regular expression literal ( `"".r` )
  - any character (wildcard) ( `any` )
  - character set ( `set(...)` )
- `rule` combinator
  - used in right-hand side of non-teriminal
- the other combinators
  - zero or one ( `e.?` )
  - zero or more ( `e.*` )
  - one or more ( `e.+` )
  - interleaving ( `repead0By`, `repeat1By` )
  - handling left-associativity ( `chainl` )
  - methods to use for-comprehension ( `map` , `flatMap` `filter` )
- the combinators for better error reporting
  - `commit` and `withErrorMessage` , to convert the failure to error parser

## Install SComb

Add the following line to your `build.sbt`

```scala
libraryDependencies += "com.github.kmizu" %% "scomb" % "0.9.0"
```

## Getting Started

You can write your own parser by inheriting
 `com.github.kmizu.scomb.SCombinator[T]` :

```scala
import com.github.kmizu.scomb.SCombinator

object IntegerParser extends SCombinator[Int] {
  override def root: P[Int] = rule {
    (digit.*).map{ case digits => digits.mkString.toInt }
  }
  lazy val digit: P[String] = set('0'to'9')
  
  def main(args: Array[String]): Unit = {
    assert(parse("100") == Success(100))
  }
}
```

In this example, `P[Int]` indicates that the parse result is `Int` .
`digit` defined using `set` combinator matches one of character from `[0-9]`.  `digit.*` matches the repetition
of `digit` and the result is translated to `Int` by `map{ case digits => digits.mkString.toInt }` .  Finally,
a rule must be enclosed by `rule { ... }` combinator.

## More Information

Some examples are below:

- [CalculatorSpec](https://github.com/kmizu/scomb/blob/v0.8.1/src/test/scala/com/github/kmizu/scomb/CalculatorSpec.scala)
  - Arithmetic Expression Parser
- [JsonSpec](https://github.com/kmizu/scomb/blob/v0.8.1/src/test/scala/com/github/kmizu/scomb/JsonSpec.scala)
  - JSON Parser
- [RegularExpressionSpec](https://github.com/kmizu/scomb/blob/v0.8.1/src/test/scala/com/github/kmizu/scomb/RegularExpressionSpec.scala)
  - (Basic) Regular Expression Parser
- [PrimitiveSpec](https://github.com/kmizu/scomb/blob/v0.8.1/src/test/scala/com/github/kmizu/scomb/PrimitiveSpec.scala)
  - Tests of primitive combinators

## Scaladoc

[Here](http://javadoc-badge.appspot.com/com.github.kmizu/scomb_2.13/com/github/kmizu/scomb/index.html)
