# ox-wk.el

[![Build Status](https://travis-ci.org/w-vi/ox-wk.el.svg?branch=master)](https://travis-ci.org/w-vi/ox-wk.el)
[![MELPA](https://melpa.org/packages/ox-wk-badge.svg)](https://melpa.org/#/ox-wk)

Emacs org-mode publishing backend for exporting org files to Dokuwiki
and Creole wiki formats.

It is a derived backend and uses ox-html as a base so anything which
is not yet covered by this backend will be exported as a html code.

## Requirements

- Emacs 24

- Org mode 8.3


## Supported features

Only limited number of elements are supported,the basic ones,
used at least by me most of the time. See `test/test.org` for the features.

This package provides two commands for export, depending on the desired output:

* `ox-wk-export-as-wiki` on a temporary buffer

* `ox-wk-export-to-wiki` to a ".txt" file.

### Org elements

-   headings

-   bold, italics and so on

-   links (including inline images)

-   plain lists (ordered, unordered)

-   nested lists

-   tables (headers only in columns so far)

-   example and src blocks

-   verbatim

## Installation

`ox-wk.el` is available on [Melpa](https://melpa.org):

`M-x package-install ox-wk`

or copy the file to wherever your Emacs lisp code resides, typically  ~/.emacs.d/site-lisp/
and add this to your .emacs file possibly close to your org-mode settings.

    (require 'ox-wk)

### Optional configurations

Set wiki syntax style, default is Dokuwiki so to set it to Creole add
the line below to you .Emacs file.

    (setq ox-wk-style 'creole)


Set Dokuwiki preference for  org = and ~ markup. It can be exported
either as verbatim or as fixed width block. I haven't figured out yet how
to distinguish these 2 so it is optional. Set it to 'monospace or 'verbatim.

    (setq ox-wk-org-verbatim 'monospace) ;; use monospace markup
    (setq ox-wk-org-verbatim 'verbatim) ;; use nowiki markup


## Development

It's just one file but to make sure everything works there is a simple CI test
in place using travis-ci.org see Makefile for more details.


## Acknowledgments

This backend is heavily inspired by others like ox-md,
ox-latex, ox-html or ox-confluence as well as by other parts of
almighty org-mode.
