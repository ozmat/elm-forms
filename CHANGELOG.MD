# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.0.1] - 2018-08-28
### Added
- Support for Elm 0.19

### Removed
- Real world example ([issue #1](https://github.com/ozmat/elm-forms/issues/1))

## [2.0.0] - 2018-07-22 [Latest release for Elm 0.18]
### Added
- `Forms` that re-exports the common types and functions
- `Forms.Validation.Result` with `ConfigError` and `FormResult`
- Field getters and setters in `Forms.Form`
- New validation helper : `Forms.Validation.isChecked`
- New validate helper : `Forms.Validation.discardable`
- Real world example (using Bootstrap, external api etc.)
- More documentation (and comments in the examples)

### Changed
- API design : now using opaque types and internals 
- Refactor `Forms.Field.Group` to `Forms.Field.Fields`
- Refactor field creation helpers to `input`, `select` and `checkbox`
- Basic validation helpers now take errors as parameters
- Refactor type validation helpers (to `stringField` and `boolField`)
- Refactor side-effects functions in `Forms.Update`
  - Use explicit funtion names
  - Now returns `(Model, Cmd)` instead of `Cmd`
- Refactor `Forms.Validation.optional` : move the `default` parameter
- Refactor `Forms.Validation.passwordMatch` : run the validation function first then test equality
- Refactor `Forms.Validation.optionalMaybe` to `Forms.Validation.optionalWithMaybe`
- Refactor `Forms.Validation.fieldGroup` to `Forms.Validation.fieldgroup`
  
### Removed
- All the basic validation `FieldError` (basic validation helpers now take errors as parameters)

## [1.1.0] - 2018-02-06
### Added
- `Forms.Validation.toResult`

## 1.0.0 - 2018-02-05
### Added
- Initial release

[Unreleased]: https://github.com/ozmat/elm-forms/compare/2.0.1...HEAD
[2.0.1]: https://github.com/ozmat/elm-forms/compare/2.0.0...2.0.1
[2.0.0]: https://github.com/ozmat/elm-forms/compare/1.1.0...2.0.0
[1.1.0]: https://github.com/ozmat/elm-forms/compare/1.0.0...1.1.0
