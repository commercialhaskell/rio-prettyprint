# Changelog for rio-prettyprint

## 0.1.5.0

* Add `Pretty` instances for `SomeBase Dir` and `SomeBase File`.

## 0.1.4.0

* Add `string` and `mkNarrativeList`.
* The `Show` instance of `PrettyException` is now derived. `displayException` is
  now defined, as the `displayException` of the inner exception.

## 0.1.3.0

* Add `SimplePrettyApp`, `mkSimplePrettyApp` and `runSimplePrettyApp`, which
  facilitate the provision, and use, of a basic environment including pretty
  printing functionality.
* Add `PrettyException` representing pretty exceptions.

## 0.1.2.0

* Expose data constructor of StyleDoc
  [#8](https://github.com/commercialhaskell/rio-prettyprint/pull/8)

## 0.1.1.0

* Add `Debug`, `Info` and `OtherLevel` data constructors to type `Style`
  (intended to be used like the existing `Warning` and `Error` constructors) and
  a `logLevelToStyle` function.
* Add `Secondary` and `Highlight` data constructors to type `Style`.
* `defaultStyles` now includes defaults for the new `Style` values,
  corresponding to those used by the `rio` package in its logging output.

## 0.1.0.0

* Initial stable release
