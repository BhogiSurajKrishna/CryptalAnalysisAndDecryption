#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )
(define (g a l)
  (if (null? l) (cons a 0)
  (if (equal? a (car l)) (cons a (+ 1 (cdr (g a (cdr l)))))
      (cons a (cdr (g a (cdr l)))))))

(define (rem-mult l n)
  (if (null? l) l
         (if (not (= (length (car l)) n)) (rem-mult (cdr l) n)
             (append (list (car l)) (rem-mult (cdr l) n)))))
(define (carlst l)
  (if (null? l) l
      (append (list (list->string (caar l))) (carlst (cdr l)))))

(define (common-words cipher-word-list n)
 (define lst (map (lambda (x) (string->list x)) cipher-word-list))
  (define sing (rem-mult lst n))
  (define li (flatten sing))
  (define lis (remove-duplicates sing))
  (define lsts (map (lambda (x) (g x sing)) lis))
  (define sorted (sort lsts (lambda (x y) (> (cdr x) (cdr y)))))
  (carlst sorted))
(define (ngr l n)
  (if (< (length l) n) '()
      (append (list (drop-right l (- (length l) n))) (ngr (cdr l) n))))

(define (cipher-ngrams cipher-word-list n)
(define (ngra l)
  (if (< (length l) n) '()
      (append (list (drop-right l (- (length l) n))) (ngra (cdr l)))))
(define (ngram word)
  (ngra (string->list word)))
(define (ngrams wo-lst)
  (append* (map (lambda (x) (ngram x)) wo-lst)))


(define (frelst a l)
  (if (null? l) (cons a 0)
      (if (equal? (car l) a) (cons a (+ 1 (cdr (frelst a (cdr l)))))
          (cons a (cdr (frelst a (cdr l)))))))

(define (freq l)
  (define l1 (remove-duplicates l))
  (map (lambda (x) (frelst x l)) l1))
(define (sort-freq l)
  (sort (freq l) (lambda (x y) (> (cdr x) (cdr y)))))
(define (carlst l)
  (if (null? l) l (append (list (caar l)) (carlst (cdr l)))))
(carlst (sort-freq (ngrams cipher-word-list))))

  
;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
 
(define (remove-dup a l)
  (if (null? l) l
  (if (char=? a (car l)) (remove-dup a (cdr l))
      (append (list (car l)) (remove-dup a (cdr l))))))
(define (remov l)
  (if (null? l) '()
  (append (list (car l)) (remov (remove-dup (car l) l)))))

(define (frelst a l)
  (if (null? l) (cons a 0)
      (if (char=? (car l) a) (cons a (+ 1 (cdr (frelst a (cdr l)))))
          (cons a (cdr (frelst a (cdr l)))))))
(define (freq l)
  (define l1 (remov l))
  (map (lambda (x) (frelst x l)) l1))
(define (removed l)
  (if (null? l) '()
      (if (or (< (char->integer (car l)) 97) (> (char->integer (car l)) 122))
          (removed (remove (car l) l)) (append (list (car l)) (removed (cdr l))))))
(define (sort-freq l)
  (sort (freq l) (lambda (x y) (> (cdr x) (cdr y)))))
(define (carlst l)
  (if (null? l) l (append (list (caar l)) (carlst (cdr l)))))
(carlst (sort-freq (removed (string->list ciphertext)))))


;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-bigrams cipher-word-list)
(define g (cipher-ngrams cipher-word-list 2))
  (map (lambda (x) (list->string x)) g))


;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  
(define (sorted l)
  (sort l (lambda (x y) (> (cdr x) (cdr y)))))
(define (f a bilst)
  (if (null? bilst) (cons a 0)
  (if (equal? a (caar bilst)) (cons a (+ 1 (cdr (f a (cdr bilst)))))
      (cons a (cdr (f a (cdr bilst)))))))
(define (g a bilst)
  (if (null? bilst) (cons a 0)
  (if (equal? a (cadar bilst)) (cons a (+ 1 (cdr (g a (cdr bilst)))))
      (cons a (cdr (g a (cdr bilst)))))))
(define (h a bilst)
  (if (null? bilst) (cons a 0)
  (if (or (equal? a (caar bilst)) (equal? a (cadar bilst))) (cons a (+ 1 (cdr (h a (cdr bilst)))))
      (cons a (cdr (h a (cdr bilst)))))))
  (define ls (map (lambda (x) (string->list x)) cipher-bigrams-list))
  (define lsts (remove-duplicates ls))
  (define lst (append* ls))
  (define uni (remove-duplicates lst))
  (cond [(equal? mode 'predecessor) (sorted (map (lambda (x) (f x lsts)) uni))]
        [(equal? mode 'successor) (sorted (map (lambda (x) (g x lsts)) uni))]
        [(equal? mode 'both) (sorted (map (lambda (x) (h x lsts)) uni))]))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  
(define (sorted l)
  (sort l (lambda (x y) (> (cdr x) (cdr y)))))
(define (f a bilst)
  (if (null? bilst) (cons a 0)
  (if (equal? a (caar bilst)) (cons a (+ 1 (cdr (f a (cdr bilst)))))
      (cons a (cdr (f a (cdr bilst)))))))
(define (g a bilst)
  (if (null? bilst) (cons a 0)
  (if (equal? a (cadar bilst)) (cons a (+ 1 (cdr (g a (cdr bilst)))))
      (cons a (cdr (g a (cdr bilst)))))))
(define (h a bilst)
  (if (null? bilst) (cons a 0)
  (if (or (equal? a (caar bilst)) (equal? a (cadar bilst))) (cons a (+ 1 (cdr (h a (cdr bilst)))))
      (cons a (cdr (h a (cdr bilst)))))))
(define ls (append* (map (lambda (x) (ngr (string->list x) 2)) cipher-bigrams-list)))
  (define lst (append* ls))
  (define uni (remove-duplicates lst))
  (cond [(equal? mode 'predecessor) (sorted (map (lambda (x) (f x ls)) uni))]
        [(equal? mode 'successor) (sorted (map (lambda (x) (g x ls)) uni))]
        [(equal? mode 'both) (sorted (map (lambda (x) (h x ls)) uni))]))

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
 (define j (cipher-ngrams cipher-word-list 3))
(map (lambda (x) (list->string x)) j)) 
;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
 (define r (cipher-ngrams cipher-word-list 4))
  (map (lambda (x) (list->string x)) r))

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
 (common-words cipher-word-list 1) )

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  (common-words cipher-word-list 2))

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  (common-words cipher-word-list 3))

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  (common-words cipher-word-list 4))

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
 (define cilsts (map (lambda (x) (string->list x)) cipher-word-list)) (define (kth-eles l)
    (if (null? l) '()
        (append (list (caar l)) (kth-eles (cdr l)))))  
  (define (frelst a l)
  (if (null? l) (cons a 0)
      (if (equal? (car l) a) (cons a (+ 1 (cdr (frelst a (cdr l)))))
          (cons a (cdr (frelst a (cdr l)))))))
(define (freq l)
  (define l1 (remove-duplicates l))
  (map (lambda (x) (frelst x l)) l1))
(define (sort-freq l)
  (sort (freq l) (lambda (x y) (> (cdr x) (cdr y)))))
 (define (calst l)
   (if (null? l) '()
       (append (list (caar l)) (calst (cdr l)))))
  (calst (sort-freq (kth-eles cilsts))))

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  (define g (map (lambda (x) (list->string (reverse (string->list x)))) cipher-word-list))
  (cipher-common-initial-letters g))

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
   (define (doubl-collect l)
    (if (< (length l) 2) '()
        (if (equal? (car l) (cadr l)) (append (list (list (car l))) (doubl-collect (cdr l))) (doubl-collect (cdr l)))))
  (define (doubles lists)
    (append* (map (lambda (x) (doubl-collect (string->list x))) lists)))
   (define (frelst a l)
  (if (null? l) (cons a 0)
      (if (equal? (car l) a) (cons a (+ 1 (cdr (frelst a (cdr l)))))
          (cons a (cdr (frelst a (cdr l)))))))
(define (freq l)
  (define l1 (remove-duplicates l))
  (map (lambda (x) (frelst x l)) l1))
(define (sort-freq l)
  (sort (freq l) (lambda (x y) (> (cdr x) (cdr y)))))
  (define (calst l)
   (if (null? l) '()
       (append (list (caar l)) (calst (cdr l)))))
  (append* (calst (sort-freq (doubles cipher-word-list)))))
