1.2.1

* Fix emitting symbols that can look like numeric literals without quotes

1.2

* Rename Key constructor to Sym
* Fixed parsing/emitting of escape sequences in strings and symbols
* Add #autorun and #undef constructs found in later games

1.1.1.1

* Fix variables with certain characters being incorrectly parsed as symbols
* Fix description of new-style encryption in usage text

1.1.1

* Fix parsing various non-alphanumeric characters in unquoted keywords
* Add separate binary->text mode for new DTBs seen in Fantasia: Music Evolved

1.1.0.1

* Fix handling of newlines in printing and lexing `.dta`

1.1

* Fixed GH encryption typo in executable
* Removed RB3/Magma serialization modules
* Ported some `.dta` syntax fixes from `onyx`
* Emit `.dta` files with unquoted keywords if possible

1.0

* Initial version published on Hackage
