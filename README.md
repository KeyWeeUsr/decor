# decor
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

This library attempts to simplify removal of all frame (window) decorations as
a global minor mode.

## How to

Install it from [Melpa](https://melpa.org/#/getting-started) or clone and
install manually, then simply `M-x decor-mode`.

Automatical usage on start can be enabled in your config file with:

```elisp
(use-package decor
  :ensure t
  :config (decor-mode))
```

Alternatively you can use one of these public functions:

* `decor-toggle-single-frame(win-id on)`
* `decor-toggle-all-frames(on)`
* `decor-all-frames-on()`
* `decor-all-frames-off()`

Documentation on those can be found via `C-h f <name>`.

[melpa-badge]: http://melpa.org/packages/decor-badge.svg
[melpa-package]: http://melpa.org/#/decor
[melpa-stable-badge]: http://stable.melpa.org/packages/decor-badge.svg
[melpa-stable-package]: http://stable.melpa.org/#/decor
[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
[ci-badge]: https://github.com/KeyWeeUsr/decor/actions/workflows/test.yml/badge.svg
[ci-workflow]: https://github.com/KeyWeeUsr/decor/actions/workflows/test.yml
[cover-badge]: https://coveralls.io/repos/github/KeyWeeUsr/decor/badge.svg?branch=master
[cover-link]: https://coveralls.io/github/KeyWeeUsr/decor?branch=master
