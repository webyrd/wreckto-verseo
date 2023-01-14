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
      ;; TODO add remaining expr cases
      )))

(define valueo
  (lambda (v)
    (conde
      ((variableo v))
      ((head-valueo v)))))

(define head-valueo
  (lambda (hnf)
    (conde
      ((integero hnf))
      ((primopo hnf))
      ((tupleo hnf))
      ((fresh (x e)
         (== `(lambda (,x) ,e) hnf)
         (symbolo x)
         (expressiono e))))))

(define tupleo
  (lambda (t)
    (fresh (v*)
      (== `(tuple . ,v*) t)
      (list-of-valueso v*))))

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
