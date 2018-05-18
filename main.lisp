;; P01 (*) Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)
(defun my-last (list)
  (if (null list)
    nil
    (if (null (cdr list))
      (car list)
      (my-last (cdr list)))))

;; P02 (*) Find the last but one box of a list.
;; Example:
;; * (my-but-last '(a b c d))
;; (C D)
(defun my-but-last (list)
  (if (null list)
    nil
    (if (null (cddr list))
      list
      (my-but-last (cdr list)))))

;; P03 (*) Find the K'th element of a list.
;; The first element in the list is number 1.
;; Example:
;; * (element-at '(a b c d e) 3)
;; C
(defun element-at (list pos)
  (if (eq 1 pos)
    (car list)
    (element-at (cdr list) (- pos 1))))


;; P04 (*) Find the number of elements of a list.
(defun my-length (list)
  (labels ((rlength (rem n)
             (if (null rem)
               n
               (rlength (cdr rem) (1+ n)))))
    (rlength list 0)))

;; P05 (*) Reverse a list.
(defun my-reverse (list)
  (labels ((rreverse (rem acc)
             (if (null rem)
               acc
               (rreverse (cdr rem) (cons (car rem) acc)))))
    (rreverse list ())))

;; P06 (*) Find out whether a list is a palindrome.
(defun palindromep (list)
  (equalp list (my-reverse list)))

;; P07 (**) Flatten a nested list structure.
;; Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
;;
;; Example:
;; * (my-flatten '(a (b (c d) e)))
;; (A B C D E)
;;
;; Hint: Use the predefined functions list and append.
;;
;; This first version uses let/dolist.. while we could have simply used
;; recursion
;;
;; (defun my-flatten (list)
;;   (let (acc)
;;     (dolist (e list acc)
;;       (if (listp e)
;;         (setq acc (append acc (rflatten e)))
;;         (setq acc (append acc (list e)))))))
(defun my-flatten (list)
  (if (null list)
    nil
    (if (listp (car list))
      (append
        (my-flatten (car list))
        (my-flatten (cdr list)))
      (append
        (cons (car list) nil)
        (my-flatten (cdr list))))))

;; P08 (**) Eliminate consecutive duplicates of list elements.
;; If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
;;
;; Example:
;; * (compress '(a a a a b c c a a d e e e e))
;; (A B C A D E)
(defun compress (list)
  (labels ((rcompress (clist last)
           (if (null clist)
             nil
             (if (equalp last (car clist))
               (rcompress (cdr clist) last)
               (append
                 (cons (car clist) nil)
                 (rcompress (cdr clist) (car clist)))))))
    (rcompress list nil)))

;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.
;;
;; Example:
;; * (pack '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))
(defun pack (list)
  (let (result group)
    (progn
      (dolist (e list result)
        (if (equalp e (car group))
          (push e group)
          (progn
            (if (not (null group))
              (setq result (append result (cons group nil))))
            (setq group (list e)))))
      (append result (cons group nil)))))

;; P10 (*) Run-length encoding of a list.
;; Use the result of problem P09 to implement the so-called run-length encoding
;; data compression method. Consecutive duplicates of elements are encoded as
;; lists (N E) where N is the number of duplicates of the element E.
;;
;; Example:
;; * (encode '(a a a a b c c a a d e e e e))
;; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
;;
;; The following code has been copied from P9, and adapted so that instead of
;; appending `group to the list of results, it pre-processes it as requested
;;
;; (defun encode (list)
;;   (let (result group)
;;     (progn
;;       (dolist (e list result)
;;         (if (equalp e (car group))
;;           (push e group)
;;           (progn
;;             (if (not (null group))
;;               (setq result (append
;;                              result
;;                              (cons (list (length group) (car group)) nil))))
;;             (setq group (list e)))))
;;       (append result (cons (list (length group) (car group)) nil)))))
(defun encode (list)
  (mapcar (lambda (group)
            (list (length group) (car group)))
          (pack list)))

;; P11 (*) Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element has no
;; duplicates it is simply copied into the result list. Only elements with
;; duplicates are transferred as (N E) lists.
;;
;; Example:
;; * (encode-modified '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))
(defun encode-modified (list)
  (mapcar (lambda (group)
            (if (= 1 (length group))
              (car group)
              (list (length group) (car group))))
          (pack list)))

;; P12 (**) Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P11.
;; Construct its uncompressed version.
(defun decode (list)
  (labels ((explode (n e)
                    (if (= 0 n)
                      nil
                      (append (list e) (explode (- n 1) e))))
           (rdecode (l)
                    (if (null l)
                      nil
                      (if (listp (car l))
                        (append (explode (caar l) (cadar l)) (decode (cdr l)))
                        (cons (car l) (decode (cdr l)))))))
    (rdecode list)))

;; P13 (**) Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method
;; directly. I.e. don't explicitly create the sublists containing the
;; duplicates, as in problem P09, but only count them. As in problem P11,
;; simplify the result list by replacing the singleton lists (1 X) by X.
;;
;; Example:
;; * (encode-direct '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))
(defun encode-direct (list)
  (flet ((encode-group (group)
                       (if (= 1 (length group))
                         (car group)
                         (list (length group) (car group)))))
    (let (result group)
      (progn
        (dolist (e list result)
          (if (equalp e (car group))
            (push e group)
            (progn
              (if (not (null group))
                (setq result (append result (cons (encode-group group) nil))))
              (setq group (list e)))))
        (append result (cons (encode-group group) nil))))))

;; P14 (*) Duplicate the elements of a list.
;; Example:
;; * (dupli '(a b c c d))
;; (A A B B C C C C D D)
(defun dupli (list)
  (if (null list)
    nil
    (cons (car list) (cons (car list) (dupli (cdr list))))))

;; P15 (**) Replicate the elements of a list a given number of times.
;; Example:
;; * (repli '(a b c) 3)
;; (A A A B B B C C C)
(defun repli (list n)
  (labels ((rrepli (l n c)
           (cond
             ((null l) nil)
             ((zerop c) (rrepli (cdr l) n n))
             (t (cons (car l) (rrepli l n (decf c)))))))
    (rrepli list n n)))

;; P16 (**) Drop every N'th element from a list.
;; Example:
;; * (drop '(a b c d e f g h i k) 3)
;; (A B D E G H K)
(defun drop (list n)
  (labels ((rdrop (l n c)
           (cond
             ((null l) nil)
             ((= 1 c) (rdrop (cdr l) n n))
             (t (cons (car l) (rdrop (cdr l) n (decf c)))))))
    (rdrop list n n)))
;; P17 (*) Split a list into two parts; the length of the first part is given.
;; Do not use any predefined predicates.
;;
;; Example:
;; * (split '(a b c d e f g h i k) 3)
;; ( (A B C) (D E F G H I K))
(defun split (list n)
  (labels ((wrap (list n)
             (if (>= n 0)
               n
               (wrap list (+ n (length list)))))
           (rsplit (list n acc)
                   (cond
                     ((zerop n) (cons acc (cons list nil)))
                     ((null list) (list acc))
                     (t (rsplit
                          (cdr list)
                          (decf n)
                          (append acc (list (car list))))))))
    (rsplit list (wrap list n) nil)))

;; P18 (**) Extract a slice from a list.
;; Given two indices, I and K, the slice is the list containing the elements
;; between the I'th and K'th element of the original list (both limits
;; included). Start counting the elements with 1.
;;
;; Example:
;; * (slice '(a b c d e f g h i k) 3 7)
;; (C D E F G)
(defun slice (list i k)
  (cond
    ((null list) nil)
    ((> i 1)
     (slice (cdr list) (decf i) (decf k)))
    ((> k 0)
     (cons (car list) (slice (cdr list) 0 (decf k))))))

;; P19 (**) Rotate a list N places to the left.
;;
;; Examples:
;; * (rotate '(a b c d e f g h) 3)
;; (D E F G H A B C)
;;
;; * (rotate '(a b c d e f g h) -2)
;; (G H A B C D E F)
;;
;; Hint: Use the predefined functions length and append, as well as the result
;; of problem P17.
(defun rotate (list n)
  (let ((slices (split list n)))
    (append (cadr slices) (car slices))))

;; P20 (*) Remove the K'th element from a list.
;;
;; Example:
;; * (remove-at '(a b c d) 2)
;; (A C D)
(defun remove-at (list n)
  (cond
    ((null list) nil)
    ((= n 1) (cdr list))
    ((> n 1)
     (cons (car list) (remove-at (cdr list) (decf n))))))

;; P21 (*) Insert an element at a given position into a list.
;;
;; Example:
;; * (insert-at 'alfa '(a b c d) 2)
;; (A ALFA B C D)
(defun insert-at (obj list n)
  (cond
    ((null list) nil)
    ((= n 1) (cons obj list))
    ((> n 1)
     (cons (car list) (insert-at obj (cdr list) (decf n))))))

;; P22 (*) Create a list containing all integers within a given range.
;;
;; If first argument is smaller than second, produce a list in decreasing order.
;; Example:
;; * (range 4 9)
;; (4 5 6 7 8 9)
(defun range (from to)
  (labels ((rrange (from to)
                   (if (> from to)
                     nil
                     (cons from (rrange (incf from) to)))))
    (if (> from to)
      (reverse (rrange to from))
      (rrange from to))))

;; P23 (**) Extract a given number of randomly selected elements from a list.
;;
;; The selected items shall be returned in a list.
;; Example:
;; * (rnd-select '(a b c d e f g h) 3)
;; (E D A)
;;
;; Hint: Use the built-in random number generator and the result of problem P20.
(defun rnd-select (list n &optional with-repetitions)
  (cond
    ((null list) nil)
    ((zerop n) nil)
    (t
      (let ((pos (+ 1 (random (length list)))))
        (cons
          (element-at list pos)
          (if with-repetitions
            (rnd-select list (decf n))
            (rnd-select (remove-at list pos) (decf n))))))))

;; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;;
;; The selected numbers shall be returned in a list.
;; Example:
;; * (lotto-select 6 49)
;; (23 1 17 33 21 37)
;;
;; Hint: Combine the solutions of problems P22 and P23.
(defun lotto-select (n range-max)
  (rnd-select (range 1 range-max) n))

;; P25 (*) Generate a random permutation of the elements of a list.
;;
;; Example:
;; * (rnd-permu '(a b c d e f))
;; (B A D C E F)
;;
;; Hint: Use the solution of problem P23.
(defun rnd-permu (list)
  (rnd-select list (length list)))

;; P26 (**) Generate the combinations of K distinct objects chosen from the
;; N elements of a list In how many ways can a committee of 3 be chosen from
;; a group of 12 people? We all know that there are C(12,3) = 220 possibilities
;; (C(N,K) denotes the well-known binomial coefficients). For pure
;; mathematicians, this result may be great. But we want to really generate all
;; the possibilities in a list.
;;
;; Example:
;; * (combination 3 '(a b c d e f))
;; ((A B C) (A B D) (A B E) ... )
(defun combination (n list)
  (cond
    ((or (> n (length list)) (<= n 0)) nil)
    ((= n 1) (mapcar #'list list))
    (t (loop for i in (range 1 (length list))
             append (loop for nested in (combination (- n 1)
                                                     (slice list
                                                            (+ i 1)
                                                            (length list)))
                          when (= (- n 1) (length nested))
                          collect (cons (element-at list i) nested))))))
