# Changelog for rio-prettyprint

## 0.1.1.0

* Add `Debug`, `Info` and `OtherLevel` data constructors to type `Style` (intended to be used like the existing `Warning` and `Error` constructors) and a `logLevelToStyle` function.
* Add `Secondary` and `Highlight` data constructors to type `Style`.
* `defaultStyles` now includes defaults for the new `Style` values, corresponding to those used by the `rio` package in its logging output.

## 0.1.0.0

* Initial stable release
