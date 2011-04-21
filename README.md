# pallet.stevedore

An embedding of shell script in clojure

See [reference documentation](http://pallet.github.com/stevedore/autodoc/index.html)
and [annotated source](http://pallet.github.com/stevedore/marginalia/uberdoc.html).

## Installation

stevedore is distributed as a jar, and is available in the
[sonatype repository](http://oss.sonatype.org/content/repositories/releases/org/cloudhoist).

Installation is with maven or your favourite maven repository aware build tool.

### lein/cake project.clj

    :dependencies [[org.cloudhoist/stevedore "0.5.0"]]
    :repositories {"sonatype"
                   "http://oss.sonatype.org/content/repositories/releases"}

### maven pom.xml

    <dependencies>
      <dependency>
        <groupId>org.cloudhoist</groupId>
        <artifactId>pallet-common</artifactId>
        <version>0.5.0</version>
      </dependency>
    <dependencies>

    <repositories>
      <repository>
        <id>sonatype</id>
        <url>http://oss.sonatype.org/content/repositories/releases</url>
      </repository>
    </repositories>

## License

Licensed under [EPL](http://www.eclipse.org/legal/epl-v10.html)

Copyright 2010, 2011 Hugo Duncan.
