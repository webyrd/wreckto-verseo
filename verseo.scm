(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")

 ;; should prolly use CLP(SMT) for arithmetic

(define verseo
  (lambda ()
    'TODO
    ))


;; Grammar from fig 1 on page 3

(define integero
  (lambda (k)
    (numbero k)))

(define variableo
  (lambda (x)
    (symbolo x)))

(define programo
  (lambda (prog)
    (fresh (e)
      ;; TODO ensure fvs(e) is empty
      (== `(one ,e) prog)
      (expressiono e))))

(define expressiono
  (lambda (expr)
    (conde
      ((valueo expr))
      ((appo expr))
      ((equalo expr))
      ((seqo expr))
      ((existo expr))
      ((alternateo expr))
      ((failo expr))
      ((oneo expr))
      ((allo expr)))))

(define valueo
  (lambda (v)
    (conde
      ((variableo v))
      ((head-valueo v)))))

(define appo
  (lambda (expr)
    (fresh (v1 v2)
      (== `(app ,v1 ,v2) expr)
      (valueo v1)
      (valueo v2))))

(define equalo
  (lambda (expr)
    (fresh (e1 e2)
      (== `(= ,e1 ,e2) expr)
      (expressiono e1)
      (expressiono e2))))

(define seqo
  (lambda (expr)
    (fresh (e1 e2)
      (== `(seq ,e1 ,e2) expr)
      (expressiono e1)
      (expressiono e2))))

(define existo
  (lambda (expr)
    (fresh (x e)
      (== `(exist (,x) ,e) expr)
      (symbolo x)
      (expressiono e))))

(define alternateo
  (lambda (expr)
    (fresh (e1 e2)
      (== `(alt ,e1 ,e2) expr)
      (expressiono e1)
      (expressiono e2))))

(define failo
  (lambda (expr)
    (== 'fail expr)))

(define oneo
  (lambda (expr)
    (fresh (e)
      (== `(one ,e) expr)
      (expressiono e))))

(define allo
  (lambda (expr)
    (fresh (e)
      (== `(all ,e) expr)
      (expressiono e))))

(define head-valueo
  (lambda (hnf)
    (conde
      ((integero hnf))
      ((primopo hnf))
      ((tupleo hnf))
      ((lambdao hnf)))))

(define tupleo
  (lambda (hnf)
    (fresh (v*)
      (== `(tuple . ,v*) hnf)
      (list-of-valueso v*))))

(define lambdao
  (lambda (hnf)
    (fresh (x e)
      (== `(lambda (,x) ,e) hnf)
      (symbolo x)
      (expressiono e))))

(define list-of-valueso
  (lambda (v*)
    (conde
      ((== '() v*))
      ((fresh (v v-rest)
         (== `(,v . ,v-rest) v*)
         (valueo v)
         (list-of-valueso v-rest))))))

(define primopo
  (lambda (op)
    (conde
      ((== 'gt op))
      ((== 'add op)))))
