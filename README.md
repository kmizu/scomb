## SComb: Simple but Powerful Parser Combinator Library in Scala
 
[![Build Status](https://travis-ci.org/kmizu/scomb.png?branch=master)](https://travis-ci.org/kmizu/scomb)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.kmizu/scomb_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.kmizu/scomb_2.12)
[![Scaladoc](http://javadoc-badge.appspot.com/com.github.kmizu/scomb_2.12.svg?label=scaladoc)](http://javadoc-badge.appspot.com/com.github.kmizu/scomb_2.12/index.html#com.github.kmizu.scomb.package)
[![Reference Status](https://www.versioneye.com/java/com.github.kmizu:scomb_2.11/reference_badge.svg?style=flat)](https://www.versioneye.com/java/com.github.kmizu:scomb_2.12/references)

SComb (su-ko-n-bu) is a simple but powerful parser combinator library written in Scala.
SComb aims to replace [scala-parser-combinators](https://github.com/scala/scala-parser-combinators).

## Features of SComb (0.1):

- primitive combinators
  - string literal
  - regular expression literal
  - any character (wildcard)
  - character set
- the other combinators
  - zero or one ( `e.?` )
  - zero or more ( `e.*` )
  - one or more ( `e.+` )
  - interleaving ( `repead0By`, `repeat1By` )
  - handling left-associativity ( `chainl` )
  - methods to use for-comprehension ( `map` , `flatMap` `filter` )
- the combinators for better error reporting
  - `fatal` , to convert the failure to fatal error

## Usage of SComb

Add the following line to your `build.sbt`

```scala
libraryDependencies += "com.github.kmizu" %% "scomb" % "0.1"
```

See [Scaladoc](http://javadoc-badge.appspot.com/com.github.kmizu/scomb_2.12/index.html#com.github.kmizu.scomb.package)
