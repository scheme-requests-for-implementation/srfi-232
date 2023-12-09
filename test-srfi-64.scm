;;; SPDX-FileCopyrightText: 2022 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

;;; Portable R7RS test framework for SRFI 64.

(import (scheme base)
        (scheme case-lambda)
        (srfi 64))

(include "srfi-232.scm")
(include "test-body.scm")
