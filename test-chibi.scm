;;; SPDX-FileCopyrightText: 2022 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

;;; Test framework for chibi-scheme.

(import (scheme base)
        (scheme case-lambda)
        (chibi test))

;;; SRFI 64 shim

;; chibi's test-equal is not SRFI 64's test-equal.
(define-syntax test-equal
  (syntax-rules ()
    ((test-equal . rest)
     (test . rest))))

(define-syntax test-eqv
  (syntax-rules ()
    ((test-eqv . rest)
     (test-equal eqv? . rest))))

(include "srfi-232.scm")
(include "test-body.scm")
