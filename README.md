[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/sideline.svg)](https://jcs-emacs.github.io/jcs-elpa/#/sideline)

# sideline
> Show informations on the side

[![CI](https://github.com/jcs-elpa/sideline/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/sideline/actions/workflows/test.yml)

This library provides the frontend UI to display information either on the
left/right side of the buffer window.

*P.S. The implementation is extracted and modified from [lsp-ui-sideline](https://github.com/emacs-lsp/lsp-ui#lsp-ui-sideline)*

## 🔨 Quickstart

```elisp
(leaf sideline
  :init
  (setq sideline-backends-skip-current-line t  ; don't display on current line
        sideline-order-left 'down              ; or 'up
        sideline-order-right 'up               ; or 'down
        sideline-format-left "%s   "           ; format for left aligment
        sideline-format-right "   %s"          ; format for right aligment
        sideline-priority 100))                ; overlays' priority
```

### Configure backends

The most basic way to set up the backends for sideline.

```elisp
(leaf sideline
  :init
  (setq sideline-backends-left '(sideline-flycheck)
        sideline-backends-right '(sideline-lsp)))
```

<p align="center">
<img src="./etc/1.png" width="70%"/>
</p>

Alternatively, you could set it to cons cell with the search order.

```elisp
(leaf sideline
  :init
  (setq sideline-backends-right '((sideline-lsp      . up)
                                  (sideline-flycheck . down))))
```

<p align="center">
<img src="./etc/2.png" width="70%"/>
</p>

## 📌 Define your own backend

Following is an example code to define your own sideline backend:

```elisp
(defun my-backend (command)
  "Example backend."
  (cl-case command
    (`candidates '("info 1" "info 2" "info 3"))  ; required
    (`action (lambda (candidate &rest _)   ; optional
               (message "Execute command for `%s`!" candidate)))))
```

or define it asynchronously:

```elisp
(defun my-backend-async (command)
  "Example async backend."
  (cl-case command
    (`candidates (cons :async (lambda (callback &rest _)
                                (funcall callback '("info 1" "info 2" "info 3")))))
    (`action ...)))
```

then you can tell your user to...

```elisp
(setq sidelin-backends-left '(my-backend))  ; use `sidelin-backends-right' for right alignment
```

Here is a list of supported commands:

* `candidates` - list of strings to display; accept async function
* `action` - callback function after the mouse click
* `face` - face overrides the default sideline face

## 📂 Example projects

* [sideline-flycheck](https://github.com/jcs-elpa/sideline-flycheck)
* [sideline-lsp](https://github.com/jcs-elpa/sideline-lsp)

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
