#lang Racket
;; Basic board format:
(define empty-board '((#f #f #f) (#f #f #f) (#f #f #f)))
(define numbered-board '((1 2 3) (4 5 6) (7 8 9)))
(define solved-board '((x o o) (o x x) (o o x)))

;; Flattened board format:
;; '(1 2 3 4 5 6 7 8 9)

(define (transpose m) (apply map list m))
;; turns a basic board into a flattened-board.
;; BasicBoard -> FlatBoard
(define (flatten-board b)
  (apply append b))

;; m n FlatBoard -> BasicBoard (size m x n)
;;(define (unflatten-board m n l)
;; (cond
;;    (m)))

;; Prints each row of the board on a new line.
;; BasicBoard -> Void
(define (show-board b)
  (cond
    ([null? (cdr b)] (println (car b)))
    (#t (println (car b))
        (show-board (cdr b)))))

;; Returns x if all elements of the list are equal, otherwise returns #f
(define (all-equal? x)
  (cond
    ([null? (cdr x)] x)
    ([equal? (car x) (cadr x)] (all-equal? (cdr x)))
    (#t #f)))
  
;; Returns a list of winners in current board b.
;; BasicBoard (3x3) -> ListOf Symbols
(define (check-win board)
  (let ([combos (append board 
                        (transpose board)
                        (list (list (car (car board)) 
                                    (cadr (cadr board))
                                    (caddr (caddr board))))
                        (list (list (cadr (cdar board))
                                    (cadr (cadr board))
                                    (caar (cddr board)))))])
       (map car 
            (filter (lambda (n) (car n))
                    (map all-equal? combos)))))

;;(show-board numbered-board)
;;(check-win solved-board)
