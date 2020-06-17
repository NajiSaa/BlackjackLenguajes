#lang planet neil/sicp
;; Naji M A Saadat
;; Proyecto final de blackjack

(define INTRO_MESSAGE
 "Welcome and play BLACKJACK.\n
 The goal is to get higher than the dealer without passing 21..\n
 RULES:
 J Q K are considered as tens.\n
 A can be 1 or 11.\n
 The other cards 2 3 4 5 6 7 8 9 have the value of the number.\n
 if the dealer has below 17, he asks for another card.\n
 if more the dealer stays.\n
 Instructions: Each player is given two open cards. .\n
 The dealer receives one open card and one hidden card.\n
 Either Hit, which means you receive another card, or Stay with your cards.\n"
)

(define (dealer-finishes-game deck dealer player)
  (newline)
  (display "Dealer shows\n")
  (display dealer)
  (newline)
  (display (calculate-hand-value dealer))
  (cond 
    ((> (calculate-hand-value dealer) 21) "Dealer Busts! Winner")
    ((and (= 21 (calculate-hand-value dealer)) 
      (= 21 (calculate-hand-value player))) "EQUAL")
    ((or (= 21 (calculate-hand-value dealer)) 
       (> (calculate-hand-value dealer) (calculate-hand-value player))) "You Lose!!")
    (else (< (calculate-hand-value dealer) 17) 
      (begin 
        (set! dealer (cons (car deck) dealer))
        (set! deck (cdr deck))
        (newline)
        (display "Dealer Hits")
        (dealer-finishes-game deck dealer player)
      )
    )
  )
)                                                                                       

(define (game-loop deck dealer player)
  (display " HIT or STAY ?\n")
  (let ((answer (read)))
    (cond (
       (eq? answer 'hit) 
         (begin 
          (set! player (cons (car deck) player))
           (display player)
           (newline)
           (display (calculate-hand-value player))
           (cond ((and (> (calculate-hand-value player) 21) (newline) "BUST! YOU LOSE!"))
             ((< (calculate-hand-value player) 21) 
                (begin 
                   (newline)
                   (set! deck (cdr deck))
                   (game-loop deck dealer player)
                )
              )
               (else (game-loop deck dealer player))
            )
          )
        )
        ((eq? answer 'stay)
         (cond ((and (equal? (calculate-hand-value player) 21) (= 2 (length player)))
                     "BLACKJACK! WINNER WINNER CHICKEN DINNER! ")
               ((= (calculate-hand-value player) 21) (dealer-finishes-game deck dealer player))
               (else (dealer-finishes-game deck dealer player))
         )
        )
        (else "fail"))))

;; main method execute this
(define (start-playing)
  (display INTRO_MESSAGE)
  (let ((deck (shuffle complete-deck)))
    (let ((dealer (cons (car deck) (cons (cadr deck) '())))
          (player (cons (caddr deck) (cons (cadddr deck) '()))))
      (display "Dealer has ") (display (car dealer)) (display "\n")
      (display "You show ") (display player)(display "\n") (display (calculate-hand-value player)) (newline)
      (game-loop (cddddr deck) dealer player)
    )
  )
)

;; Should be thte alternal or the normal
(define (whichCount count alt-count)
  (if (>= 21 count) 
    count
    alt-count
  )
)

;; Adjunt only one ace can be 11
(define (adjust-for-aces aces count alt-count)
  (newline)
  (if (>= aces 1)
      (cond 
        ((= aces 1) 
          (whichCount (+ 11 count) (+ 1 alt-count))
        )
        ((= aces 2) 
          (whichCount (+ 12 count) (+ 2 alt-count))
          
        )
        ((= aces 3) 
          (whichCount (+ 13 count) (+ 3 alt-count))
        )
        ((= aces 4) 
           (whichCount (+ 14 count) (+ 4 alt-count))
        )
      )
      count
    )
)

;; Calculate the hand value
(define (calculate-hand-value hand)
  (let (
      (count 0) 
      (alt-count 0)
    )
    (define (iterator hand-1)
      (cond ((null? hand-1) (adjust-for-aces (count-aces hand) count alt-count))
            ((eq? (caar hand-1) 'A) (iterator (cdr hand-1)))   
            ((or 
              (eq? (caar hand-1) 'K)
              (eq? (caar hand-1) 'Q)
              (eq? (caar hand-1) 'J)) 
                (begin (set! count (+ count 10))
                (set! alt-count (+ alt-count 10))
                (iterator (cdr hand-1))))
            (else (begin (set! count (+ (caar hand-1) count))
                (set! alt-count (+ (caar hand-1) alt-count))
                (iterator (cdr hand-1))))))
      (iterator hand)
    )
  )
 
;; Constant of the cards.
(define suit-types
  '(Spade Heart Diamond Club)
)
(define deck-value
  '(A 2 3 4 5 6 7 8 9 10 J Q K)
)

;; Create the deck
(define (generate-deck deck-values suits)
 (define (iterator list1 list2)
    (define (iterator-2 list-1 list-2)
      (if (null? list-2) (iterator (cdr list1) list2)
        (append 
          (list (cons (car list-1) (car list-2))) 
          (iterator-2 list-1 (cdr list-2))
        )
      )
    )
    (if (null? list1) 
      nil
      (iterator-2 list1 list2)
    )
 )
 (iterator deck-values suits)
)

(define complete-deck
  (generate-deck deck-value suit-types)
)

;; Method to shuffle the cards
(define (shuffle lista)
  (define (loop in out n)
    (if (= n 0) 
        (cons (car in) (shuffle (append (cdr in) out)))
        (loop (cdr in) (cons (car in) out) (- n 1))
    )
  )
  (if (null? lista)
    '()
    (loop lista '() (random (lista-length lista)))
  )
)

;; Counter of the hand
  (define (count-aces hand)
    (cond ((null? hand) 0)
      (
        (equal? (caar hand) 'A) 
          (+ 1 (count-aces (cdr hand)))
      )
      (else (count-aces (cdr hand)))
    )
  )

;; Count the size of the list
(define (lista-length lista)            
  (if (null? lista)                 
      0                              
      (+ (lista-length (cdr lista)) 1 )
  )
) 

