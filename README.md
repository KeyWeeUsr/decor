[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

# decor

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
