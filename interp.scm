(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################

        
        (newarray-exp (exp1 exp2)
                    (let ((lenght (expval->num (value-of exp1 env)))
                          (value (value-of exp2 env)))
                      (arr-val (array-creation lenght  value))))
                          
                    
        
        (updatearray-exp (exp1 exp2 exp3)
                         (let ((array (expval->arr (value-of exp1 env)))
                               (index (expval->num (value-of exp2 env)))
                               (value (value-of exp3 env)))
                           (array-update array index value)
                     
                       ))

        (readarray-exp (exp1 exp2)
                       (let ((array (expval->arr (value-of exp1 env)))
                             (index (expval->num (value-of exp2 env))))
                         (array-read array index)
                     ))
        (printarray-exp (exp1)
                        (let ((array (expval->arr (value-of exp1 env))))
                          (begin
                            (display "[")
                            (display " ")
                            (array-print array))
                            
                          
                     ))
        (newstack-exp () ;(newarray-exp stack-max-lenght empty-value))
                      (arr-val (array-creation stack-max-lenght  empty-value)))

        (stackpush-exp (exp1 exp2)
                       (let ((stack (expval->arr (value-of exp1 env)))
                             (value (value-of exp2 env)))
                         (array-update stack (+ (car stack) (find-top stack 0)) value)))

        (stackpop-exp (exp1) (let ((stack (expval->arr (value-of exp1 env))))
                         (if (equal? (find-top stack 0) 0)
                             -1
                             (let ((retval (array-read stack (- (find-top stack 0) 1))))
                           
                               
                               (begin
                             (array-update stack (- (find-top stack 0) 1) empty-value);
                             retval;
                             )
                           ))))
        (stacksize-exp (exp1) (let ((stack (expval->arr (value-of exp1 env))))
                         (num-val (find-top stack 0))))

        (stacktop-exp (exp1) (let ((stack (expval->arr (value-of exp1 env))))
                         (let ((retval (array-read stack (- (find-top stack 0) 1))))
                           retval
                           )))

        (emptystack-exp (exp1)
                        (let ((stack (expval->arr (value-of exp1 env))))
                          (if (equal? (find-top stack 0) 0)
                              (bool-val #t)
                              (bool-val #f))))

        (printstack-exp (exp1)
                        (let ((stack (expval->arr (value-of exp1 env))))
                          (begin
                            ;(display "[")
                            (stack-print stack)
                            ;(display "]")
                            )))

        (arraycomprehension-exp (exp1 var1 exp2)
                                
                                (arr-val (comp exp1 var1 (expval->arr (value-of exp2 env)) 0 env)))
                        
                        

        ; #####################################################
        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE

  (define empty-value -1)
  ;(define stack-max 10000)
  ;(define stack-min 1)
  (define stack-max-lenght 1000)

  (define (array-creation length value)
    (if (= length 0)
        '()
        (cons
         (newref value)
         (array-creation (- length 1) value))))
  
  (define (array-update array1 index value)
    
                (begin
                  ;(display (+ index  (car array1)))
                  (setref! (+ index  (car array1)) value)))

  (define (array-read array1 index)
                      (deref (+ index (car array1))))

  (define (array-print array)
    (if (null? array)
        (display "]")
        (begin
                (display (expval->num (deref (car array))))
                (display " ")
                (array-print (cdr array)))))

  (define (find-top stack index)
    (if (equal? (array-read stack index) empty-value) index (find-top stack (+ 1 index))))
                  
  (define (stack-print stack)
    (if (equal? 0 (find-top stack 0))
        (display "")
         (if (null? stack)
        (display "]")
        (if (equal? (array-read stack (car stack)) empty-value)
            (if (equal? -1 (deref (car stack)))
                (display "")
                (display (expval->num (deref(car stack))))
            
           
   
            
            
            
        )
             (begin
                (display (expval->num (deref (car stack))))
                (display " ")
                (stack-print (cdr stack)))))))

  (define (array-lenght array)
    (if (null? array)
        0
        (+ 1 (array-lenght (cdr array)))))

        
   (define (comp exp1 var array index env)
     (if (equal? index (array-lenght array))
         array
         (begin
           (array-update array index (value-of exp1 (extend-env var (array-read array index) env)));
           (comp exp1 var array (+ 1 index) env)
           )))
     

  
                  
                
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
