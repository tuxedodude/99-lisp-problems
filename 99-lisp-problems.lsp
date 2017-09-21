(silent)

(set 'SHOW_ASSERT_MSG false )

(define-macro (assert condition (msg "Assert succeeded!"))
	(if (eval condition) (if SHOW_ASSERT_MSG (println (eval msg)))
		(println "Assertion failed: " condition)))

(define-macro (assert= expr1 expr2 (msg "Assert succeeded!"))
	(if (= (eval expr1) (eval expr2)) (if SHOW_ASSERT_MSG (println (eval msg)))
		(println "Assertion failed:") (println expr1 " != " expr2 "; result was " (eval expr1))))

(set 'x (sequence 0 10))

;; p01 find the last box of a list
(define (my-last L) (last L))

(assert= (my-last x) 10 "P01 success")

;; p02 find the last but one box of a list
(define (my-but-last L) (slice L -2))

(assert= (my-but-last '(a b c d)) '(c d) "P02 success") 

;; P03 find the K'th element of a list.
(define (element-at L n) (L (- n 1)))

(assert= (element-at '(a b c d e) 3) 'c "P03 success")

;; P04 find length of list
(define (my-length L) (length L))

(assert= (my-length x) 11 "P04 success")

;; P05 reverse a list.
(define (my-reverse L) (reverse L))

(assert= (my-reverse (my-reverse x)) x)

;; P06 find out whether a list is a palindrome
(define (palindrome? L) (= L (reverse L)))

(assert (palindrome? '(x a n a x)))

;; P07 flatten an arbitrarily nested list
(define (flatten L) (flat L))

(assert= (flatten '((a b c (d)) e f (((g))))) '(a b c d e f g))

;; P08 remove consecutive duplicates in a list
(define (compress L , (prev '()))
	(if (null? L) '()
			(let (A (first L) B (rest L))
			(if (= prev A) (compress B nil A) 
					(cons A (compress B nil A))))))

(assert= (compress '(a a a a b c c a a d e e e e)) '(a b c a d e))

;; P09------------------------

(set 'p '(a a a a b c c a a d e e e e))

;; helper: find the index of first element of L not equal to x and return its index
(define (find-new X L , (idx 0))
	(if (catch
			(dolist (E L)
				(if (!= E X) (throw idx))
				(inc idx))) 
			$it
			idx))

(assert= (find-new 'a p) 4)

;; P09 pack consecutive duplicates of list elements into sublists.
(define (my-pack L , (packlist '()) )
	(if (null? L) (reverse packlist)
		(letn (A (first L)
					 K (find-new A L))
			(push (slice L 0 K) packlist)
			(my-pack (slice L K) nil packlist))))
			
(assert= (my-pack p) '((a a a a) (b) (c c) (a a) (d) (e e e e)))

;; P10 run-length encoding of a list
(define (encode L)
	(map (fn ($x) (list (length $x) (first $x))) (my-pack L)))

(assert= (encode p) '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)))

;; P11 modified run-length encoding: if the element has no duplicates,
;; copy it into the result list.

(define (encode2 L)
	(map (fn ($x) (if (= (first $x) 1) ($x 1) $x)) (encode L)))

(assert= (encode2 p) '((4 a) b (2 c) (2 a) d (4 e)))

;;P12 (**) Decode a run-length encoded list.
;;Given a run-length code list generated as specified in problem P11. Construct its ;;uncompressed version.
(define (decode L)
	(flat (map (fn ($x) (if (atom? $x) $x (dup ($x 1) (first $x)))) L)))

(assert= (decode (encode2 p)) p)

;;P13 (**) Run-length encoding of a list (direct solution).
;;Implement the so-called run-length encoding data compression method directly. I.e. don't ;;explicitly create the sublists containing the duplicates, as in problem P09, but only count ;;them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by ;;X.

(define (RLE L , (packlist '()) )
	(if (null? L) (reverse packlist)
		(letn (A (first L)
					 K (find-new A L))
			(push (if (= 1 K) A (list K A)) packlist)
			(RLE (slice L K) nil packlist))))

(assert= (RLE p) (encode2 p))

;; P14 duplicate the elements of a list: (a b c c d) -> (a a b b c c c c d d )
(define (dupli L)
	(flat (map (fn ($x) (dup $x 2)) L)))

(assert= (dupli '(a b c c d)) '(a a b b c c c c d d))

;;P15 (**) Replicate the elements of a list a given number of times.
(define (repli L n)
	(flat (map (fn (%x) (dup %x n)) L)))

(assert= (repli '(a b c) 3) '(a a a b b b c c c))

;;P16 Drop every N'th element from a list.
;;(drop '(a b c d e f g h i k) 3)
;;'(a b d e g h k)

(define (drop lst n , acc nth-index)
	(setq acc '()
				nth-index (fn (X) (= 0 (% (+ 1 X) n))))
	(if (>= n 0)
			(dolist (x lst)
				(if (not (nth-index $idx)) 
					(push x acc -1))))
	acc)
				
(assert= (drop '(a b c d e f g h i k) 3) '(a b d e g h k))

[text]
P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that ;;there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really ;;generate all the possibilities in a list.
(define (combination k L) nil)

P17 (*) Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.

Example:
* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
[/text]

;; p26 trivial implementation
(define (trivial-split lst n)
	(list (slice lst 0 n) (slice lst n)))

(define (split lst n , A B)
	(setq A '() B '())
	(if (< n 0) (set 'n (+ n (length lst))))
	(dolist (x lst)
		(if (< $idx n) (push x A -1)
				(push x B -1)))
	(list A B))

(assert= (trivial-split '(a b c d e f g h i k) 3)  '((a b c) (d e f g h i k)))
(assert= (split '(a b c d e f g h i k) 3)  '((a b c) (d e f g h i k)))

[text]
P18 (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

Example:
* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)
[/text]

(define (within a b x)
	(let (a (min a b) b (max a b))
		(and (>= x a) (< x b))))

(assert (within 0 10 5))
(assert (within 10 0 5))
(assert= (within 1 5 5) false)
(assert (within 1 5 1))
(assert (within 1 5 4))
(assert= false (within 1 5 0))

;; P18: using baked in slice is too easy.
(define (my-slice lst start end , acc)
	(dolist (X lst)
		(if (within (- start 1) end $idx) (push X acc -1)))
	acc)

(assert= (my-slice '(a b c d e f g h i k) 3 7)
				'(c d e f g))

[text]
P19 (**) Rotate a list N places to the left.
Examples:
* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)

Hint: Use the predefined functions length and append, as well as the result of problem P17.[/text]

(define (rotate-left lst N)
	(let (p (split lst N))
		(append (p 1) (p 0))))

(assert= (rotate-left '(a b c d e f g h) 3) '(d e f g h a b c))
(assert= (rotate-left '(a b c d e f g h) -2) '(g h a b c d e f))

[text]
P20 (*) Remove the K'th element from a list.
Example:
* (remove-at '(a b c d) 2)
(A C D)
[/text]

(define (remove-at lst K)
	(append (slice lst 0 (- K 1)) (slice lst K)))

(assert= (remove-at '(a b c d) 2) '(a c d))

[text]
P21 (*) Insert an element at a given position into a list.
Example:
* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
[/text]

(define (insert-at ele lst N)
	(append 
		(append (slice lst 0 (- N 1)) 
						(list ele)) (slice lst (- N 1))))

(assert= (insert-at 'alfa '(a b c d) 2) '(a alfa b c d))
	
[text]
P22 (*) Create a list containing all integers within a given range.
If first argument is smaller than second, produce a list in decreasing order.
Example:
* (range 4 9)
(4 5 6 7 8 9)
[/text]

;; p22 is trivial; use (sequence ...)

[text]
P23 (**) Extract a given number of randomly selected elements from a list.
The selected items shall be returned in a list.
Example:
* (rnd-select '(a b c d e f g h) 3)
(E D A)

Hint: Use the built-in random number generator and the result of problem P20.
[/text]

(define (rnd-select lst N , (acc '()) )
	(dotimes (I N)
		(let (_i (rand (length lst)))
			(push (lst _i) acc)
			(set 'lst (remove-at lst (+ 1 _i) ))))
	acc)

(assert= (sort (rnd-select '(a b c d e f g h) 8)) '(a b c d e f g h))

[text]
P24 (*) Lotto: Draw N different random numbers from the set 1..M.
The selected numbers shall be returned in a list.
Example:
* (lotto-select 6 49)
(23 1 17 33 21 37)

Hint: Combine the solutions of problems P22 and P23.[/text]

(define (lotto-select n M)
	(rnd-select (sequence 1 M) n))

(assert= 8 (length (lotto-select 8 1000)))
(assert= 1 (length (lotto-select 1 100)))
(letn (n 50 lst (lotto-select n 100))
	(assert= (length lst) (length (unique lst))))
(assert= (sort (lotto-select 10 10)) (sequence 1 10))

[text]
P25 (*) Generate a random permutation of the elements of a list.
Example:
* (rnd-permu '(a b c d e f))
(B A D C E F)

Hint: Use the solution of problem P23.[/text]

(define (rnd-permu lst) (rnd-select lst (length lst)))

(define (test-rnd-permu)
	(letn (A '(a b c d e f) B (rnd-permu A))
		(apply and (map (fn (X) (find X B)) A))))

(assert (test-rnd-permu))
(assert= (sort (rnd-permu '(a b c d e f))) '(a b c d e f))

[text]
P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example:
* (combination 3 '(a b c d e f))
((A B C) (A B D) (A B E) ... )[/text]

(define (combinations N lst , I)
	(if (< N 0) nil
			(= N 0) '()
			(= N 1) (map list lst)
			true
		(let (I (- N 1))
			(if (or (>= I (length lst)) (< N 2)) '()
				(let (A (slice lst 0 I) B (slice lst I))
					(append (combine A B) (combinations N (rest lst))))))))

(define (combine elems lst)
	(if (null? lst) '()
			(append (list (append elems (list (first lst))))
							(combine elems (rest lst)))))

(assert= (combinations 2 '(a b c)) '((a b) (a c) (b c)) )
(assert= (combinations 2 '(a b)) '((a b)) )
(assert= (combinations 1 '(a b c)) '((a) (b) (c)) )
(assert= (combinations 0 '(a b c)) '())
(assert= (combinations 1 '(a)) '((a)))
(assert= (combinations 3 '(a b c)) '((a b c)))
(assert= (combinations 3 '(a b)) '())

(silent)

(print "evaluated")
