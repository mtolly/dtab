dtab, DTA/DTB toolkit by onyxite (tolly@wisc.edu).

Heavily built on previous programs & research by:

* xorloser (ArkTool & DtbCrypt)
* deimos (dtb2dta)
* other members of ScoreHero (http://scorehero.com)

Version 0.1: initial version.

Version 0.5: all basic functionality.

* Done: encrypt+decrypt (old+new crypt), dta2dtb, dtb2dta.
* Done: support reading/writing from both files and stdio.
* Todo: extensive testing on dta/dtb corpus.
* Todo: proper encoding support (UTF-8? Latin-1?)
* Todo: modify dta lexer & parser to print source locations in errors.
* Todo: verify characters permissible in raw keywords.
  Maybe, just allow any characters which aren't parsed as something else?

Version 0.6: lexer & parser now print source locations.

Version 0.7: supports decoding/encoding for both UTF-8 and Latin-1.

* Todo: write a new lexer and pretty-printer which work with Text instead of
  String.

Version 0.8:

* Removed UTF-8 support. This doesn't actually exist; all Harmonix DTA files are
in Latin-1.
* Removed all usage of Data.Text. Because all strings are Latin-1, they are now
simply stored as ByteStrings.
* New parser using Happy. Make sure to use at least Happy v1.18.7; this version
fixed a bug involving partial parse errors.
