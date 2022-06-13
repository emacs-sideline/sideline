[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# sideline
> Show informations on the side

[![CI](https://github.com/jcs-elpa/sideline/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/sideline/actions/workflows/test.yml)

## Usage

```elisp
(defun sideline-my-backend (command)
  "Example of sideline backend."
  (cl-case command
    (`candidates '("info 1" "info 2" "info 3"))
    (`action (lambda (candidate)
               ;; Do something..
               ))))
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
