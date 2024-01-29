;;; SPDX-License-Identifier: MIT
;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe
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

;;; Adapted from the R7RS all-syntax-rules version.  ER macros are
;;; needed for MIT/GNU Scheme, since it doesn't support (v1 v2 ... . vn)
;;; patterns in syntax-rules.

(define-syntax curried
  (syntax-rules ()
    ((curried formals exp ...)
     (curried-1 formals (begin exp ...)))))

(define-syntax curried-1
  (er-macro-transformer
    (lambda (exp inject same?)
      (let ((formals (cadr exp)) (body (caddr exp)))
        (cond ((null? formals) body)
              ((symbol? formals) (list (inject 'lambda) formals body))
              ((dotted-list? formals)
               `(,(inject 'letrec)
                 ((,(inject 'f)
                    (,(inject 'case-lambda)
                      (() ,(inject 'f))
                      (,formals ,body)
                      (,(inject 'args)
                       (more-args ,(inject 'f) ,(inject 'args))))))
                  ,(inject 'f)))
              ((proper-list? formals)
               `(,(inject 'letrec)
                 ((,(inject 'f)
                   (,(inject 'case-lambda)
                    (() ,(inject 'f))  ; app. to no args -> original function
                    (,formals ,body)
                    (,(append formals (inject 'rest))
                     (apply (,(inject 'f) ,@formals) ,(inject 'rest)))
                    (,(inject 'args)
                     (more-args ,(inject 'f) ,(inject 'args))))))
                  ,(inject 'f)))
              (else
               (syntax-error "invalid formals")))))))

(define (more-args f current)
  (lambda args (apply f (append current args))))

(define-syntax define-curried
  (syntax-rules ()
    ((define-curried (var . formals) exp ...)
     (define var
       (curried formals exp ...)))))
