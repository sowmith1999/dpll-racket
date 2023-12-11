#lang racket

; This is a simplified version of DPLL written at https://github.com/Kraks/SAT.rkt/
; Simplified to only contain forms that are supported in https://github.com/harp-lab/brouhaha

(define cnf-form (list
                  (set 1 2)
                  (set -1 -2)))

(define (unit? cls) (equal? 1 (set-count cls)))

(define (has-unit? f)
  (define unit-vars (map set-first (filter unit? f)))
  (if (empty? unit-vars) #f
      (car unit-vars)))

(define (neg var) (- 0 var))

(define (not-neg? var) (> var 0))

(define (get-var var) (abs var))

(define (remove-var v) (λ (cls) (set-remove cls v)))

(define (not-contains v) (λ (cls) (if (set-member? cls v) #f #t)))

(define (get-all-vars cls)
  (foldl set-union (set) cls))

(define (elim-unit f uv)
  (define assgn (make-immutable-hash (if (not-neg? uv) `((,uv . #t)) `((,(neg uv) . #f)))))
  (define new-f (map (remove-var (neg uv)) (filter (not-contains uv) f)))
  (values new-f assgn))

(define (has-pure? f)
  (define pure-vars
    (foldl append '()
           (filter (λ (vs) (eq? 1 (length vs)))
                   (group-by get-var (set->list (get-all-vars f))))))
  (if (empty? pure-vars) #f
      (car pure-vars)))

(define (assgn-update* a1 a2)
  (foldl (λ (kv m) (hash-set m (car kv) (cdr kv))) a1 (hash->list a2)))

(define (pick-var f) (set-first (car f)))

(define (dpll f assgn)
  (cond [(memf (compose zero? set-count) f) #f] ; checks if f has non empty clauses
        [(zero? (length f)) assgn] ; checks if the f has no clauses, then returns current assignment
        [(has-unit? f) ; checks if there are any unit clauses and eliminates them and calls dpll on the new f with new assgn
         => (λ (uv)
              (define-values (new-f new-assgn) (elim-unit f uv))
              (dpll new-f (assgn-update* assgn new-assgn)))]
        [(has-pure? f) ; checks if there are any pure, if there are calls dpll without that 
         => (λ (pv) (dpll (cons (set pv) f) assgn))]
        [else ; if none of the above, assignb a variable value and checks if satifies.
         (define v (pick-var f))
         (cond [(dpll (cons (set v) f) assgn)
                => (λ (assgn) assgn)]
               [else (dpll (cons (set (neg v)) f) assgn)])]))

(dpll cnf-form (make-immutable-hash))
