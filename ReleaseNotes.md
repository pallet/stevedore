# Stevedore Release Notes

The latest stable release is 0.7.3.

## 0.8.0-beta.4

- Use {} for grouping of negation expressions
  Fixes #25

## 0.8.0-beta.3

- Allow for unnamed functions in exception message

- Simplify src line comment handling

- Add optional keyword arguments to deref
  The keyword arguments allow support of bashes parameter substitution
  modifiers,
  -, :-, =, :=, +, :+, ?, :?.

## 0.8.0-beta.2

- Allow mulitple arguments to quote

- Remove reflection warnings

- Fix unquote splicing

## 0.8.0-beta.1

- Change groupID to com.palletops

- Use leiningen instead of maven

- Symbols in argument position are resolved
  This removes the need to deref script functions.  Script functions are 
  now regular functions, with a custom dispatch mechanism.  Plain clojure
  functions can now be used as script functions.

  Fixes #22.

## 0.8.0-alpha.1

- Update to pallet-common 0.3.1

- Add source form location in script comments
  When generating script, insert the file and line of the source forms that
  generate the script.  The file and line are inserted as comments on the
  line preceding the generated code.  This is a convenient format that
  allows script lines to be easily used in continuation lines, when
  chaining commands together for instance.

  Introduces pallet.stevedore/fragment, which is similar to p.s/script,
  except that it disables source comments and ensures pipelines are kept on
  a single line.  This is required in some expansion contexts.

  The pallet.stevedore/with-source-line-comments macro can also be used to
  control the output of source comments when using p.stevedore/script.

- Add a marker to messages for script exit status
  In order to allow grep'ing for status messages, a prefix is added to each
  message. By default, prefix script exit status messages by '#> '.  The
  prefix may be altered by binding pallet.stevedore.common/*status-marker*.

- Ensure propagation of metadata on stevedore forms
  We correctly propagate :line metadata, and add :file metadata, to each
  stevedore source form and use it in exceptions.  Removes use of slingshot
  and forces clojure 1.4+.

- Update project version and dependencies
  Also changes to use dev-resources instead of test-resources, to match
  lein2 conventions.

## 0.7.3

- Allow use of deref as a value in a doseq binding

## 0.7.2

- Simplify conditional generation
  The conditional expressions were overly complex and could generate
  incorrect code in some corner cases. This simplifies the generated
  expressions and makes them more robust

- Create a :default implementation for stevedore/emit
  Improve the error reporting associated with not binding *script-language*
  and for un-handled clojure types.

- Add slingshot version compatibility

## 0.7.1

- Make stevedore slingshot version agnostic

- Fix an issue with splice
  The unquote splicing form was being transformed into a form with a
  function literal rather than a symbol

- Update for clojure-1.3.0 compatability

## 0.7.0

- Improve logical test detection
  Logical test detection was failing when the condition was an unquoted
  clojure expression.

- Rename pallet.script.lib to pallet.script.scriptlib
  This was duplicating a namespace definied in pallet

- Make shflags completely optional
  Fixes #16.

## 0.6.0

- Add dispatching for script language using `*script-language*`

- Add initial support for windows batch scripting

- Initial support for clojure 1.3
  Mark *script-fn-dispatch* and *script-language* :dynamic

- Separated public functions and implementation details. Added documentation.

- Setup shFlags even with empty arguments, mainly for docstrings

- Removed possibility of anonymous defn scripts.

- Added declare-arguments

- Added support for flag arguments with script defn


## 0.5.0

This is the initial standalone release.  The library has been extracted
from the main [pallet repository](https://github.com/pallet/pallet).
