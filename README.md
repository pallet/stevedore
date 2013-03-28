# pallet.stevedore

An embedding of shell script in clojure.


See [reference documentation](http://palletops.com/doc/reference/0.8/script/),
[api documentation](http://pallet.github.com/stevedore/autodoc/index.html)
and [annotated source](http://pallet.github.com/stevedore/marginalia/uberdoc.html).

See [tests](https://github.com/pallet/stevedore/tree/develop/test/pallet/stevedore) for usage examples.

## Installation

stevedore is distributed as a jar, and is available in the
[clojars repository](http://clojars.org/com.palletops/stevedore).

Installation is with lein or your favourite maven repository aware build tool.

### lein project.clj

```clj
:dependencies [[com.palletops/stevedore "0.8.0-beta.2"]]
```

### maven pom.xml

```xml
<dependencies>
  <dependency>
    <groupId>com.palletops</groupId>
    <artifactId>stevedore</artifactId>
    <version>0.8.0-beta.2</version>
  </dependency>
<dependencies>

<repositories>
  <repository>
    <id>clojars</id>
    <url>http://clojars.org/repo</url>
  </repository>
</repositories>
```

## License

Licensed under [EPL](http://www.eclipse.org/legal/epl-v10.html)

Copyright 2010, 2011, 2012, 2013 Hugo Duncan.
