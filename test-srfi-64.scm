;;; SPDX-License-Identifier: MIT
;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe

;;; Portable R7RS test framework for SRFI 64.

(import (scheme base)
        (scheme case-lambda)
        (srfi 64))

(include "srfi-232.scm")
(include "test-body.scm")
