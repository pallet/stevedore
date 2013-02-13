# pallet.stevedore

An embedding of shell script in clojure

See [reference documentation](http://pallet.github.com/stevedore/autodoc/index.html)
and [annotated source](http://pallet.github.com/stevedore/marginalia/uberdoc.html).

See [tests](https://github.com/pallet/stevedore/tree/develop/test/pallet/stevedore) for usage examples.

## Installation

stevedore is distributed as a jar, and is available in the
[sonatype repository](http://oss.sonatype.org/content/repositories/releases/org/cloudhoist).

Installation is with maven or your favourite maven repository aware build tool.

### lein/cake project.clj

```clj
:dependencies [[org.cloudhoist/stevedore "0.8.0-beta.1"]]
:repositories {"sonatype"
               "http://oss.sonatype.org/content/repositories/releases"}
```

### maven pom.xml

```xml
<dependencies>
  <dependency>
    <groupId>org.cloudhoist</groupId>
    <artifactId>stevedore</artifactId>
    <version>0.8.0-beta.1</version>
  </dependency>
<dependencies>

<repositories>
  <repository>
    <id>sonatype</id>
    <url>http://oss.sonatype.org/content/repositories/releases</url>
  </repository>
</repositories>
```

## License

Licensed under [EPL](http://www.eclipse.org/legal/epl-v10.html)

Copyright 2010, 2011, 2012, 2013 Hugo Duncan.
