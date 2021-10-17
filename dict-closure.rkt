#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (append (list (cons (car l1) (car l2))) (zip (cdr l1) (cdr l2)))))
(define (small a) (if (and (> (char->integer a) 96) (< (char->integer a) 123)) #t #f))
(define (capit a) (if (and (> (char->integer a) 64) (< (char->integer a) 91)) #t #f))

(define (dictionary-closure key) 
(define key2 key)
  
(define (matched l1 l2 key)
  (define key3 key)
  (define (mat l1 l2 key key3)
  (if (not (= (length l1) (length l2))) #f
      (if (or (null? l1) (null? l2)) #t
      (if (equal? (car l1) (car l2)) (mat (cdr l1) (cdr l2) key key3)
          (if (capit (car l1)) #f
              (if  (equal? (list-ref key (- (char->integer (car l2)) 65)) '_)
                      
                  (mat (cdr l1) (cdr l2) key (utils:add-substitution (list (cons (car l2) (car l1))) key3))
               (if (utils:is-monoalphabetic? (list (cons (car l2) (car l1))) key3) (mat (cdr l1) (cdr l2) key
                                               (utils:add-substitution (list (cons (car l2) (car l1))) key3)) #f)))))))

  (mat l1 l2 key key3))
(define (math st1 st2 key3)
  (matched (string->list st1) (string->list st2) key3))
  
  (define (check word wordlist key4 a)
    (if (= a 2) '()
    (if (null? wordlist) '()
        (if (math word (car wordlist) key4) (append (list (car wordlist)) (check word (cdr wordlist) key4 (+ a 1)))
                                       (check word (cdr wordlist) key4 a)))))
  
  (define (getkey key key2 wo-list)
    (define decrypted (foldr (lambda (x y) (cons (utils:decrypt key2 x) y)) '() wo-list))
                                                                         
                                                                                                 
    (cond [(null? decrypted) key2]
          [else (cond  [(null? (filter (lambda (x) (small x)) (string->list (car decrypted))))
                                                              (getkey key key2 (cdr decrypted))]
                    [else (define check1 (check (car decrypted) utils:dictionary key2 0)) (define b (length check1))
    (if (= b 0) #f
        (if  (= b 1) (getkey key (utils:add-substitution
                                               (filter (lambda (x) (not(and (capit (car x)) (capit (cdr x)))))
                                                       (zip (string->list (car check1))
                                                                           (string->list (car decrypted)))) key2)
                                                                                                         decrypted)
                                  (getkey key key2 (cdr decrypted))) )])]))
  (getkey key key2 utils:cipher-word-list)
                    )

            



  
  