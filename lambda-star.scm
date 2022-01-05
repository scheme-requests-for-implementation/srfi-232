(define-syntax lambda*
  (syntax-rules ()
    ((lambda* formals exp ...)
     (lambda*-1 formals (begin exp ...)))))

(define-syntax lambda*-1
  (syntax-rules ()
    ((lambda*-1 () exp)
     (lambda args
       (if (null? args) exp (apply (exp) args))))
    ((lambda*-1 (arg0 arg1 ...) exp)
     (one-or-more (arg0 arg1 ...) exp))
    ((lambda*-1 (arg0 arg1 ... . rest) exp)
     (dotted (arg0 arg1 ... . rest) exp))
    ((lambda*-1 args exp) (lambda args exp))))

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

(define-syntax dotted
  (syntax-rules ()
    ((dotted (arg0 arg1 ... . rest) exp)
     (letrec ((f (case-lambda
                   (() f)
                   ((arg0 arg1 ... . rest) e)
                   (args (more-args f args)))))
       f))))

(define (more-args f current)
  (lambda args (apply f (append current args))))

(define-syntax define*
  (syntax-rules ()
    ((define* (var . formals) exp ...)
     (define var
       (lambda* formals exp ...)))))
