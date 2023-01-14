(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")

 ;; should prolly use CLP(SMT) for arithmetic

(define verseo
  (lambda ()
    'TODO
    ))


;; Grammar from fig 1 on page 3

(define expressiono
  (lambda (expr)
    'TODO
    ))

(define head-valueso
  (lambda (hnf)
    (conde
      ((numbero hnf))
      ;; TODO: op
      ;; TODO: <v1, ..., vn>
      ((fresh ()
         (== `(lambda (,x) ,e) hnf)
         (symbolo x)
         (expressiono e))))))
