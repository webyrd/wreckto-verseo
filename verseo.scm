(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")

 ;; should prolly use CLP(SMT) for arithmetic

(define verseo
  (lambda ()
    'TODO
    ))


;; Grammar from fig 1 on page 3

;; TODO fix use of 'bound-vars' to include variable reference
;; and the introduction of bound variables (lambda and exist)

(define programo
  (lambda (prog)
    (fresh (e)
      (== `(one ,e) prog)
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
    (symbolo x)))

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
      (expressiono e bound-vars))))

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
      (expressiono e bound-vars))))

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
