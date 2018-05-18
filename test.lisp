(load "main.lisp")

(defmacro test (expected actual)
  `(assert (equalp ,expected ,actual)))

;; P1
(test 'd (my-last '(a b c d)))
;; P2
(test '(c d) (my-but-last '(a b c d)))
;; P3
(test 'a (element-at '(a b c d) 1))
(test 'b (element-at '(a b c d) 2))
(test 'c (element-at '(a b c d) 3))
(test 'd (element-at '(a b c d) 4))
;; P4
(test 1 (my-length '(a)))
(test 2 (my-length '(a b)))
(test 5 (my-length '(a b c d e)))
;; P5
(test '(e d c b a) (my-reverse '(a b c d e)))
;; P6
(test T (palindromep '(a b c b a)))
(test nil (palindromep '(a b c b)))
;; P7
(test '(a b c d e) (my-flatten '(a (b (c d) e))))
;; P8
(test '(a b c a d e) (compress '(a a a a b c c a a d e e e e)))
;; P9
(test '((a a a a) (b) (c c) (a a) (d) (e e e e)) (pack '(a a a a b c c a a d e e e e)))
;; P10
(test '((4 a) (1 b) (2 c) (2 a) (1 d)(4 e)) (encode '(a a a a b c c a a d e e e e)))
;; P11
(test '((4 a) b (2 c) (2 a) d (4 e)) (encode-modified '(a a a a b c c a a d e e e e)))
;; P12
(test '(a a a a b c c a a d e e e e) (decode '((4 a) b (2 c) (2 a) d (4 e))))
;; P13
(test '((4 a) b (2 c) (2 a) d (4 e)) (encode-direct '(a a a a b c c a a d e e e e)))
;; P14
(test '(a a b b c c c c d d) (dupli '(a b c c d)))
;; P15
(test '(a a a b b b c c c) (repli '(a b c) 3))
;; P16
(test '(a b d e g h k) (drop '(a b c d e f g h i k) 3))
;; P17
(test '((a b c) (d e f g h i k)) (split '(a b c d e f g h i k) 3))
(test '((a b c d e f g h i k)) (split '(a b c d e f g h i k) 11))
;; P18
(test '(c d e f g) (slice '(a b c d e f g h i k) 3 7))
;; P19
(test '(d e f g h a b c) (rotate '(a b c d e f g h) 3))
(test '(g h a b c d e f) (rotate '(a b c d e f g h) -2))
;; P20
(test '(a c d) (remove-at '(a b c d) 2))
;; P21
(test '(a alfa b c d) (insert-at 'alfa '(a b c d) 2))
;; P22
(test '(4 5 6 7 8 9) (range 4 9))
(test '(9 8 7 6 5 4) (range 9 4))
;; P23
;; We cannot test this, as it's random!
;;(test '(e d a) (rnd-select '(a b c d e f g h) 3))
;; P24
;; We cannot test this, as it's random!
; (test '(23 1 17 33 21 37) (lotto-select 6 49))
;; P25
;; We cannot test this, as it's random!
; (test '(b a d c e f) (rnd-permu '(a b c d e f))
;; P26
(test '((a b c)
        (a b d)
        (a b e)
        (a b f)
        (a c d)
        (a c e)
        (a c f)
        (a d e)
        (a d f)
        (a e f)
        (b c d)
        (b c e)
        (b c f)
        (b d e)
        (b d f)
        (b e f)
        (c d e)
        (c d f)
        (c e f)
        (d e f)) (combination 3 '(a b c d e f)))
