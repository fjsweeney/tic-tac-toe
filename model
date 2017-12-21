#lang Racket
;; Basic board format:
(define empty-board '((#f #f #f) (#f #f #f) (#f #f #f)))
(define numbered-board '((1 2 3) (4 5 6) (7 8 9)))

;; Flattened board format:
;; '(1 2 3 4 5 6 7 8 9)

(define (transpose m) (apply map list m))
(define (flatten-board b)
  (apply append b))
(define (unflatten-board m n l)
  (cond
    (m)))


(define (show-board b)
  (cond
    ([null? (cdr b)] (println (car b)))
    (#t (println (car b))
        (show-board (cdr b)))))

(define (all-equal? x)
  (cond
    ([null? (cdr x)] x)
    ([equal? (car x) (cadr x)] (all-equal? (cdr x)))
    (#t '(#f))))
  
(define (check-win board)
  (let ([combos (append board 
                        (transpose board)
                        (list (list (car (car board)) 
                                    (cadr (cadr board))
                                    (caddr (caddr board))))
                        (list (list (cadr (cdar board))
                                    (cadr (cadr board))
                                    (caar (cddr board)))))])
       (filter (lambda (n) (car n))
               (map all-equal? combos))))

(show-board numbered-board)
