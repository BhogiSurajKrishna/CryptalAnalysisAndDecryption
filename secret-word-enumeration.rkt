#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt"))

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define words utils:dictionary)
(define (caps-to-s lst)
  (if (null? lst) '()
       (append (list (integer->char (+ (char->integer (car lst)) 32))) (caps-to-s (cdr lst)))))
(define (s-to-caps lst)
  (if (null? lst) '()
       (append (list (integer->char (- (char->integer (car lst)) 32))) (s-to-caps (cdr lst)))))
(define (semicheck lis lf)
  (define li (caps-to-s lis))
 (define (che li lf) (if (or (null? li) (null? lf)) #t
  (if (or (equal? (car lf) #\_) (equal? (car li) (car lf))) (che (cdr li) (cdr lf)) #f))) (che li lf))
      
(define (matchof str l)
  (define l1 (string->list str))
  (if (not (equal? (length l1) (length l))) #f
      (if (not (equal? (remove-duplicates l1) l1)) #f
          (if (semicheck l1 l) #t #f))))
            
(define (secret-word-enumeration key-after-dictionary-closure)
 (cond [(equal? key-after-dictionary-closure #f) #f] ;; Returns a key or false (#f)
       [else (define sw-form (drop-right key-after-dictionary-closure (- (length key-after-dictionary-closure) 6)))
 
  (define (finalcheck lst wo-list key store)
    (cond [(and (null? wo-list) (= (length store) 0)) #f]
          [(and (null? wo-list) (= (length store) 1)) (car store)]
          [(= (length store) 2) key]
          [(and (matchof (car wo-list) lst) 
                          (matchof (list->string (s-to-caps (utils:encryption-key (car wo-list)))) key))
                          (finalcheck lst (cdr wo-list) key (append (list (utils:encryption-key (car wo-list))) store))]
          [(matchof (car wo-list) lst) (finalcheck lst (cdr wo-list) key store)]
          [else (finalcheck lst (cdr wo-list) key store)]))
  (finalcheck sw-form utils:dictionary key-after-dictionary-closure '())]))
    