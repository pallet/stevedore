# Stevedore Release Notes

The latest release is 0.6.0.

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
