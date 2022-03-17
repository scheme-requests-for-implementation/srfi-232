;;; (C) 2022 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice (including the
;;; next paragraph) shall be included in all copies or substantial
;;; portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(import (scheme base)
        (scheme case-lambda))

(define-syntax curried
  (syntax-rules ()
    ((curried formals exp ...)
     (curried-1 formals (begin exp ...)))))

(define-syntax curried-1
  (syntax-rules ()
    ((curried-1 () exp)
     (lambda args
       (if (null? args) exp (apply (exp) args))))
    ((curried-1 (arg0 arg1 ...) exp)
     (one-or-more (arg0 arg1 ...) exp))
    ((curried-1 (arg0 arg1 ... . rest) exp)
     (rest-args (arg0 arg1 ... . rest) exp))
    ((curried-1 args exp) (lambda args exp))))

(define-syntax one-or-more
  (syntax-rules ()
    ((one-or-more (arg0 arg1 ...) exp)
     (letrec
      ((f (case-lambda
            (() f)       ; app. to no args -> original function
            ((arg0 arg1 ...) exp)
            ((arg0 arg1 ... . rest)
             (apply (f arg0 arg1 ...) rest))
            (args (more-args f args)))))
       f))))

(define-syntax rest-args
  (syntax-rules ()
    ((rest-args (arg0 arg1 ... . rest) exp)
     (letrec ((f (case-lambda
                   (() f)
                   ((arg0 arg1 ... . rest) exp)
                   (args (more-args f args)))))
       f))))

(define (more-args f current)
  (lambda args (apply f (append current args))))

(define-syntax define-curried
  (syntax-rules ()
    ((define-curried (var . formals) exp ...)
     (define var
       (curried formals exp ...)))))
