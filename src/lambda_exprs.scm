 (define-module (lambda-exprs)
   #:use-module (srfi srfi-1)
   #:use-module (srfi srfi-13)
   #:use-module (ice-9 pretty-print))

(define (int->string i)
  (string (integer->char i)))

(define (my-hash str size)
  (remainder (string-hash-ci str) size))
(define (my-assoc str alist)
  (find (lambda (pair) (string-ci=? str (car pair))) alist))
(define lambda-macros (make-hash-table))

(define (save-macro name expr)
  (hashx-set! my-hash my-assoc lambda-macros name expr))

(define (prepare-lambda-list l)
  (map (lambda (x) (string (integer->char x))) l))

(define (make-variable x) (list 'variable (string (integer->char x))))
(define (make-abstraction v x) `(abstraction ,(prepare-lambda-list v) ,x))
(define (make-application m n) `(application ,m ,n))
(define (define-macro name expr)
  (save-macro (int->string name) expr)
  `(macro ,(int->string name)))
(define (make-macro name)
  `(macro ,(int->string name)))
(define (variable? x) (eq? (car x) 'variable))
(define (abstraction? x) (eq? (car x) 'abstraction))
(define (application? x) (eq? (car x) 'application))
(define (macro? x) (eq? (car x) 'macro))


(define (expand-l-macro mac)
  (if (macro? mac)
      (hashx-ref my-hash my-assoc lambda-macros (cadr mac))
      mac))

(define identity '(abstraction ("x") (variable "x")))
(define lambda-true '(abstraction ("x" "y") (variable "x")))
(define lambda-false '(abstraction ("x" "y") (variable "y")))
(define lambda-free-test '(abstraction ("x" "y") (application (variable "x") (variable "z"))))

(define (free-variables x)
  (cond ((variable? x)    (list (cadr x)))
        ((application? x) (append (free-variables (cadr x))
                                  (free-variables (caddr x))))
        ((abstraction? x) (let ((bound (cadr x)))
                            (filter (lambda (var)
                                      (not (member var bound)))
                                    (free-variables (caddr x)))))
        ((macro? x)       (free-variables (expand-l-macro x)))))

(define (lambda= x y)
  (cond ((and (variable? x) (variable? y))
         (string=? (cadr x) (cadr y)))
        ((and (application? x) (application? y))
         (and (lambda= (cadr x) (cadr y))
              (lambda= (caddr x) (caddr y))))
        ((and (abstraction? x) (abstraction? y))
         (and (equal? (cadr x) (cadr y))
              (lambda= (caddr x) (caddr y))))
        ((or (macro? x) (macro? y))
         (lambda= (expand-l-macro x)
                  (expand-l-macro y)))
        (else #f)))

(define (intersection list1 list2)
  (fold (lambda (x y) (if (member x list2)
                          (cons x y)
                          y))
          '()
          list1))

(define (substitution from to expression)
  (let ((from-free (free-variables from))
        (to-free   (free-variables to))
        (expr-free (free-variables expression)))
    (cond ((macro? expression) (substitution from
                                             to
                                             (expand-l-macro expression)))
          ((lambda= from expression) to)
          ((and (variable? from)
                (not (member (cadr from) expr-free)))
           expression)
          ((application? expression)
           `(application ,(substitution from
                                        to
                                        (cadr expression))
                         ,(substitution from
                                        to
                                        (caddr expression))))
          ((and (abstraction? expression)
                (nil? (intersection (cadr expression) to-free)))
           `(abstraction ,(cadr expression)
                         ,(substitution from
                                        to
                                        (caddr expression))))
          (else expression))))

(define (modify from to list)
  (map (lambda (x) (if (string=? x from) to x)) list))

(define (alpha-conv from to expression)
  (if (and (abstraction? expression)
           (variable? from)
           (variable? to)
           (nil? (intersection (free-variables to)
                               (append
                                (cadr expression)
                                (free-variables (caddr expression))))))
      `(abstraction ,(modify (cadr from) (cadr to) (cadr expression))
                    ,(substitution from to (caddr expression)))
      (if (macro? expression)
          (alpha-conv from
                      to
                      (expand-l-macro expression))
          expression)))

(define (beta-reduce expression)
  (if (or (not (application? expression))
          (not (abstraction? (cadr expression))))
      expression
      (let* ((function (cadr expression))
             (value (caddr expression))
             (vars (cadr function))
             (var1 (car vars))
             (var-rest (cdr vars))
             (body (caddr function))
             (new-body (substitution `(variable ,var1) value body)))
        (if (not (nil? (intersection var-rest
                                     (free-variables value))))
            expression
            (if (nil? var-rest)
                new-body
                `(abstraction ,var-rest
                              ,new-body))))))

(define (reduce-step expression)
  (cond ((variable? expression) expression)
        ((and (application? expression)
              (abstraction? (cadr expression)))
         (let* ((func (cadr expression))
                (val  (caddr expression))
                (vars (cadr func))
                (collisions (intersection vars
                                          (free-variables val)))
                (new-func (fold
                           (lambda (x y) (alpha-conv `(variable ,x)
                                                     `(variable ,(string-append x "1"))
                                                     y))
                           func
                           collisions)))
           (beta-reduce `(application ,new-func ,val))))
        ((and  (application? expression)
               (macro? (cadr expression)))
         (reduce-step (make-application (expand-l-macro (cadr expression))
                                        (caddr expression))))
        ((application? expression)
         `(application ,(reduce-step (cadr expression))
                       ,(reduce-step (caddr expression))))
        ((abstraction? expression)
         `(abstraction ,(cadr expression)
                       ,(reduce-step (caddr expression))))
        (else expression)))

(define (reduce-expr expr)
  (define (reduce-expr-count expr counter)
    (let ((after (reduce-step expr)))
      (if (or (lambda= expr after)
              (> counter 15))
          after
          (reduce-expr-count after (+ counter 1)))))
  (reduce-expr-count expr 0))

(define (unwords ls)
  (reduce (lambda (y x) (string-append x " " y)) "" ls))

(define (brackets str)
  (string-append "\(" str ")"))

(define (pretty-print-expr expr)
  (cond ((variable? expr) (cadr expr))
        ((abstraction? expr) (string-append "\\"
                                            (unwords (cadr expr))
                                            "."
                                            (pretty-print-expr (caddr expr))))
        ((application? expr)
         (let ((left (if (abstraction? (cadr expr))
                         (brackets
                          (pretty-print-expr (cadr expr)))
                         (pretty-print-expr (cadr expr))))
               (right (if (variable? (caddr expr))
                          (pretty-print-expr (caddr expr))
                          (brackets
                           (pretty-print-expr (caddr expr))))))
           (unwords (list left right))))
        ((macro? expr) (string-append "#" (cadr expr)))
        (else "something went wrong")))

(define (pretty-display expr)
  (display (string-append (pretty-print-expr expr) "\n")))
