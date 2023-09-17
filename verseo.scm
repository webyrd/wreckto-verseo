(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")

 ;; should prolly use CLP(SMT) for arithmetic

(define verseo
  (lambda ()
    'TODO
    ))


;; Grammar from Fig. 1. (VC: Syntax) on page 3

(define (membero x ls)
  (fresh (y rest)
    (== `(,y . ,rest) ls)
    (conde
      ((== x y))
      ((=/= x y)
       (membero x rest)))))

(define (not-membero x ls)
  (absento x ls))

(define (fvs-expressiono expr env fvs fvs^)
  'TODO)

(define (fvs-eqo eq-expr env fvs fvs^)
  (conde
    ((fvs-expressiono eq-expr env fvs fvs^))
    ((fresh (v e fvs^^)
       (== `(= ,v ,e) eq-expr)
       (fvs-valueo v env fvs fvs^^)
       (fvs-expressiono e env fvs^^ fvs^)))))

(define (fvs-valueo value env fvs fvs^)
  (conde
    ((symbolo value)
     (conde
       ((== fvs fvs^)
        (membero value env))
       ((== `(,value . ,fvs) fvs^)
        (not-membero value env))))
    ((fvs-head-valueo value env fvs fvs^))))

(define (fvs-head-valueo head-value env fvs fvs^)
  (conde
    ((== fvs fvs^)
     (conde
       ((numbero head-value))
       ((== 'gt head-value))
       ((== 'add head-value))))
    ((fresh (v*)
       (== `(vec . ,v*) head-value)
       (fvs-vec-valueso v* env fvs fvs^)))
    ((fresh (x e)
       (== `(lam ,x . ,e) head-value)
       (symbolo x)
       (fvs-expressiono e `(,x . ,env) fvs fvs^)))))

(define (fvs-vec-valueso v* env fvs fvs^)
  (conde
    ((== '() v*) (== fvs fvs^))
    ((fresh (v v-rest fvs^^)
       (== `(,v . ,v-rest) v*)
       (fvs-valueo v env fvs fvs^^)
       (fvs-vec-valueso v-rest env fvs^^ fvs^)))))


(define programo
  (lambda (prog)
    (fresh (e)
      (== `(one ,e) prog)
      (fvso e '())
      (expressiono e '()))))

(define expressiono
  (lambda (expr bound-vars)
    (conde
      ((valueo expr bound-vars))
      ((appo expr bound-vars))
      ((equalo expr bound-vars))
      ((seqo expr bound-vars))
      ((existo expr bound-vars))
      ((alternateo expr bound-vars))
      ((failo expr bound-vars))
      ((oneo expr bound-vars))
      ((allo expr bound-vars)))))

(define valueo
  (lambda (v bound-vars)
    (conde
      ((variableo v bound-vars))
      ((head-valueo v bound-vars)))))

(define variableo
  (lambda (x bound-vars)
    (fresh ()
      (symbolo x)
      (bound-vars-lookupo x bound-vars))))

(define bound-vars-lookupo
  (lambda (x bound-vars)
    (fresh (y rest)
      (== `(,y . ,rest) bound-vars)
      (conde
        ((== x y))
        ((=/= x y)
         (bound-vars-lookupo x rest))))))

(define appo
  (lambda (expr bound-vars)
    (fresh (v1 v2)
      (== `(app ,v1 ,v2) expr)
      (valueo v1 bound-vars)
      (valueo v2 bound-vars))))

(define equalo
  (lambda (expr bound-vars)
    (fresh (e1 e2)
      (== `(= ,e1 ,e2) expr)
      (expressiono e1 bound-vars)
      (expressiono e2 bound-vars))))

(define seqo
  (lambda (expr bound-vars)
    (fresh (e1 e2)
      (== `(seq ,e1 ,e2) expr)
      (expressiono e1 bound-vars)
      (expressiono e2 bound-vars))))

(define existo
  (lambda (expr bound-vars)
    (fresh (x e)
      (== `(exist (,x) ,e) expr)
      (symbolo x)
      (expressiono e `(,x . ,bound-vars)))))

(define alternateo
  (lambda (expr bound-vars)
    (fresh (e1 e2)
      (== `(alt ,e1 ,e2) expr)
      (expressiono e1 bound-vars)
      (expressiono e2 bound-vars))))

(define failo
  (lambda (expr bound-vars)
    (== 'fail expr)))

(define oneo
  (lambda (expr bound-vars)
    (fresh (e)
      (== `(one ,e) expr)
      (expressiono e bound-vars))))

(define allo
  (lambda (expr bound-vars)
    (fresh (e)
      (== `(all ,e) expr)
      (expressiono e bound-vars))))

(define head-valueo
  (lambda (hnf bound-vars)
    (conde
      ((integero hnf bound-vars))
      ((primopo hnf bound-vars))
      ((tupleo hnf bound-vars))
      ((lambdao hnf bound-vars)))))

(define integero
  (lambda (k bound-vars)
    (numbero k)))

(define tupleo
  (lambda (hnf bound-vars)
    (fresh (v*)
      (== `(tuple . ,v*) hnf)
      (list-of-valueso v* bound-vars))))

(define lambdao
  (lambda (hnf bound-vars)
    (fresh (x e)
      (== `(lambda (,x) ,e) hnf)
      (symbolo x)
      (expressiono e `(,x . ,bound-vars)))))

(define list-of-valueso
  (lambda (v* bound-vars)
    (conde
      ((== '() v*))
      ((fresh (v v-rest)
         (== `(,v . ,v-rest) v*)
         (valueo v bound-vars)
         (list-of-valueso v-rest bound-vars))))))

(define primopo
  (lambda (op bound-vars)
    (conde
      ((== 'gt op))
      ((== 'add op)))))
