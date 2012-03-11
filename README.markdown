dtab, DTA/DTB toolkit by onyxite (tolly@wisc.edu).

Heavily built on previous programs & research by:

* xorloser (ArkTool & DtbCrypt)
* deimos (dtb2dta)
* other members of ScoreHero (http://scorehero.com)

Version 0.1: initial version.

Version 0.5: all basic functionality.

* done: encrypt+decrypt (old+new crypt), dta2dtb, dtb2dta.
* done: support reading/writing from both files and stdio.
* todo: extensive testing on dta/dtb corpus.
* todo: verify encoding support (utf-8? latin-1?)
* todo: modify dta lexer & parser to print source locations in errors.
* todo: verify characters permissible in raw keywords.
  maybe, just allow any chars which aren't parsed as something else?

Version 0.6: lexer & parser now print source locations.
