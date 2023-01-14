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
    'TODO
    ))

(define valueo
  (lambda (v)
    (conde
      ((variableo v))
      ((head-valueo v)))))

(define head-valueo
  (lambda (hnf)
    (conde
      ((integerso hnf))
      ((primopo hnf))
      ;; TODO: <v1, ..., vn>
      ((fresh ()
         (== `(lambda (,x) ,e) hnf)
         (symbolo x)
         (expressiono e))))))

(define primopo
  (lambda (op)
    (conde
      ((== 'gt op))
      ((== 'add op)))))
