#lang racket

;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (position a l)
  (pos a l 1)) (define (pos a l n) (if (null? l) (+ (length l) n) (if (equal? (car l) a) n (pos a (cdr l) (+ n 1))))) 
(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (append (list (cons (car l1) (car l2))) (zip (cdr l1) (cdr l2)))))
(define (c-p l1 l2) (cartesian-product l1 l2))

(define (etai key) 
  (define char-frq (stats:cipher-monograms utils:ciphertext))
  (define sin-list (map (lambda (x) (car (string->list x))) (stats:cipher-common-words-single utils:cipher-word-list))) 
  (define top5 (drop-right char-frq (- (length char-frq) 5)))
(define remtop (remove (car sin-list) (remove (cadr sin-list) top5)))
(define (carlst l)
  (if (null? l) '() (append (list (caar l)) (carlst (cdr l)))))
(define (sorting lst1 lst)
     (if (null? lst) '()
     (if (not (equal? (remove (car lst) lst1) lst1)) (append (list (car lst)) (sorting lst1 (cdr lst))) (sorting lst1 (cdr lst)))))
(define sremtop (reverse (sorting remtop (carlst (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both)))))
  
  (define l3 (cond [(= (length sremtop) 3) (list (cons #\E (caddr sremtop)) (cons #\E (cadr sremtop)) (cons #\E (car sremtop)))]
                   [else (list (cons #\E (cadddr sremtop)) (cons #\E (caddr sremtop)) (cons #\E (cadr sremtop)) (cons #\E (car sremtop)))]))

  (define l2 (filter (lambda (x) (not (equal? (cdar x) (cdadr x))))
            (c-p (map (lambda (x) (cons #\T x)) sremtop) l3)))

(define g (list (list (cons #\A (car sin-list)) (cons #\I (cadr sin-list))) (list (cons #\I (car sin-list)) (cons #\A (cadr sin-list)))))
  (define l5 (if(< (position (car sin-list) (stats:cipher-monograms utils:ciphertext))
                                                    (position (cadr sin-list) (stats:cipher-monograms utils:ciphertext))) g (reverse g)))
                    
(define freq-ord (drop-right (stats:cipher-monograms utils:ciphertext) (- (length (stats:cipher-monograms utils:ciphertext)) 5)))
(define double (stats:cipher-common-double-letters utils:cipher-word-list))
  (define l6 (remq* (remq* double freq-ord) freq-ord))
 (define l7 (if (> (length l6) 1) (filter (lambda (x) (not (equal? (cdar x) (cdadr x))))
                                          (cartesian-product (map (lambda (x) (cons #\E x)) l6) (map (lambda (x) (cons #\T x)) l6))) '()))

  (define l81 (string->list (car (stats:cipher-trigrams utils:cipher-word-list))))
  (define l82 (if (= 1 (length (stats:cipher-trigrams utils:cipher-word-list))) '()
                               (string->list (cadr (stats:cipher-trigrams utils:cipher-word-list)))))
  (define l8 (append (filter (lambda (y) (not (equal? (cdar y) (cdadr y))))
                                         (cartesian-product (map (lambda (x) (cons #\T x)) l81) (map (lambda (x) (cons #\E x)) l81)))
                     (filter (lambda (y) (not (equal? (cdar y) (cdadr y))))
                                         (cartesian-product (map (lambda (x) (cons #\T x)) l82) (map (lambda (x) (cons #\E x)) l82)))))
 
(define l1 (c-p l5 (if (< (length remtop) 5) l2
                        (append l7 l8))))
(define substis (map (lambda (x) (append* x)) (if (and (null? l7) (null? l8)) (c-p l5 l2) l1))) substis)                                             


                            


;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (etai (build-list 26 (lambda (x) #\_)))
                       )
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))

