# Change Log

## [v0.1.0] (2024-12-30)

This release probably still has bugs but it seems to display all 2024 Advent of Code puzzles correctly,
which is the point of the 0.1.0 release.

- Fixed detection of lots of operators like `<` and `>`
- `%=` and `%%=` are detected correctly
- Identify variables and punctuation like `,` and `:`

## [v0.0.6] (2024-12-11)

- Fixed some issues with arbitrary base literals not getting parsed correctly
- Correctly detect augmented assignment operators like `++=`
- Detect `<>` and `++` as special non arithmetic operators

## [v0.0.5] (2024-12-11)

- Fixed an issue with `.` operator's capture groups incorrectly parsing methods as operators

## [v0.0.4] (2024-12-10)

- Added support for `<=>` and `>=<` operators
- Added support for `\` floor division operator
- Fixed/improved support for `..` operator (it was getting incorrectly parsed as a dot call)

## [v0.0.3] (2024-12-02)

- Added support for the fat arrow `=>` operator in function declaration

## [v0.0.2] (2024-12-02)

- Fixed highlighting of `fn` keyword when not followed by a space
- Added highlighting of function calls when no parameters are supplied: `foo.my_fn`

## [v0.0.1]

- Initial release
