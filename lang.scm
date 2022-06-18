(module lang (lib "eopl.ss" "eopl")                

  ;; language for EXPLICIT-REFS
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
        ("letrec"
          (arbno identifier "(" identifier ")" "=" expression)
           "in" expression)
        letrec-exp)
      
      ;; new for explicit-refs

      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      (expression
        ("newref" "(" expression ")")
        newref-exp)

      (expression
        ("deref" "(" expression ")")
        deref-exp)

      (expression
        ("setref" "(" expression "," expression ")")
        setref-exp)

      ; #####################################################
      ; ###### ENTER YOUR CODE HERE
      ; ###### define the grammar definitions for new expressions here
      ; #####################################################
      (expression
        ("newarray" "(" expression "," expression ")")
        newarray-exp)
      (expression
        ("update-array" "(" expression "," expression "," expression ")")
        updatearray-exp)
      (expression
        ("read-array" "(" expression "," expression ")")
        readarray-exp)
      (expression
        ("print-array" "(" expression ")")
        printarray-exp)

      (expression
        ("newstack" "(" ")")
        newstack-exp)

      (expression
       ("stack-push" "(" expression  "," expression ")")
       stackpush-exp)

      (expression
       ("stack-pop" "(" expression ")" )
       stackpop-exp)

      (expression
       ("stack-size" "(" expression ")" )
       stacksize-exp)

      (expression
       ("stack-top" "(" expression ")" )
       stacktop-exp)

      (expression
       ("empty-stack?" "(" expression ")" )
       emptystack-exp)

      (expression
       ("print-stack" "(" expression ")" )
       printstack-exp)

      (expression
       ("[" expression "for" identifier "in" expression "]")
       arraycomprehension-exp)

      ; #####################################################

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
