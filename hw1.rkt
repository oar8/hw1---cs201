#lang racket

(provide hours
         bin2dec
         dec2bin
         hex2dec
         dec2hex
         range
         remove-p
         collect-p
         starts-with?
         contains?
         power-set
         most-movies
         )

; Please do not modify the lines above this one.

; ********************************************************
; CS 201 HW #1  DUE Wednesday, 9/20/2017 at 11:59 pm
;                via the submit system on the Zoo

; ********************************************************
; Name: Olivia Roth
; Email address: olivia.roth@yale.edu
; ********************************************************

; This file may be opened in DrRacket.
; Lines beginning with semicolons are comments.

; If you are asked to write a procedure, please make sure it has 
; the specified name, and the specified number and order of arguments.  
; The names of the formal arguments need not be the same
; as in the problem specification.

; For each problem, the intended inputs to your procedures
; are specified (for example, "positive integers") 
; and your procedures need not do anything reasonable 
; for other possible inputs.

; You may write auxiliary procedures in addition to
; the requested one(s) -- for each of your auxiliary 
; procedures, please include a comment explaining
; what it does, and giving an example or two.

; You may also use procedures you have written 
; elsewhere in this assignment or previous assignments.
; They only need to be defined once somewhere within this file.

; Please use the predicate equal? to test equality
; of values that may not be numbers.  To test equality
; of numbers, you can use =.

; Reading: Chapters 3 and 4 of the Racket Guide.

; ********************************************************
; ** problem 0 ** (1 easy point) 
; Replace the number 0 in the definition below to indicate
; the number of hours you spent doing this assignment
; Decimal numbers (eg, 6.237) are fine.  Exclude time
; spent reading.

(define hours 12)

; ********************************************************
; ** problem 1 ** (9 points)
; Write a procedure

; (bin2dec '(1 0 0))

; that takes a list of binary digits and returns the
; corresponding decimal number

; Examples
; (bin2dec '(1 0 1 0)) => 10
; (bin2dec '(1 1 1 1 1)) => 31
; (bin2dec '(1)) => 1
; (bin2dec '()) => 0
; (bin2dec '(1 0 0 0 0 0 0 0)) => 128
;
; ********************************************************
; I want this to go through the code one number at a time starting from the furthest left
(define (bin2dec num)
  (bin2decASSIST num (- (length num) 1) 0));passes the binary number, the number of items in the list minus one
;(to give you the eventual exponent you need to calculate the decimal number), and the deimal number

(define (bin2decASSIST binList place sum);takes the list of binary numbers, an integer denoting the exponent, and the decimal sum. It goes through binList, adding their decimal value to sum
  (if (equal? (length binList) 0);have we gone through all the given digits and is the binary list empty?
      sum;then return the sum you've accumulated
      (if (equal? (car binList) 1);if the first item in the list is a one, 
          (bin2decASSIST (cdr binList) (- place 1) (+ sum (expt 2 place)));pass the rest of the list through the
          ;recursive method again. Decrease 'place' (which is the number that denotes the expoential value of that
          ;item in the list) by one, increase the sum by 2 to the power of 'place'
          (bin2decASSIST (cdr binList) (- place 1) sum)));pass the rest of the list through the recursive method
  ;again. Decrease 'place' (which is the number that denotes the expoential value of that item in the list) by one
  )
; ********************************************************
; ** problem 2 ** (10 points)
; Write a procedure

; (dec2bin n)

; that takes a positive integer as input, and returns 
; a list of binary digits
; The following identity should hold:

;; (bin2dec (dec2bin n)) == n

; Examples

; (dec2bin 23) => '(1 0 1 1 1)
; (dec2bin 2) => '(1 0)
; (dec2bin 128) => '(1 0 0 0 0 0 0 0)
; (dec2bin 0) => '(0)
; (dec2bin 127) => '(1 1 1 1 1 1 1)

; ********************************************************

(define (dec2bin n)
  (dec2binASSIST n (list)))

(define (dec2binASSIST decNum binList);this method takes the decimal number and a list that records each binary number calculated
  (if (equal? decNum 0);if the decimal number is 0, end the recursive loop
      binList
      (dec2binASSIST (quotient decNum 2) (append (list(remainder decNum 2)) binList));otherwise, pass the quotient of the decimal
      ;number and binList with with the remainder of decNum and 2 appended to the front 
      )
  )
  

; ********************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (hex2dec n)

; that takes a list of hexadecimal (base 16) digits and returns the
; corresponding decimal number

; Examples

; (hex2dec '(A)) => 10
; (hex2dec '(F F)) => 255
; (hex2dec '(1 0 0)) => 256
; (hex2dec '(d e a d b e e f)) => 3735928559

; ********************************************************

(define (hex2dec num)
  (hex2decASSIST num (- (length num) 1) 0))

(define (hex2decASSIST hexList place sum);this takes the list of hexadecimal numbers, the 'place' which
  ;denotes which exponential value of 16 to use, and the sum of all the calculations thus far
  (if (equal? (length hexList) 0);if the list of hexadecimal digits is empty, return the sum you calculated
      sum
      (if (equal? (car hexList) 0);because 0 does not change the value of the number, don't change the
          ;sum and pass the rest of the list through this method again
          (hex2decASSIST (cdr hexList) (- place 1) sum)
          ;Take the remaining items (cdr) in the list and pass them through as the new 'hexList.'
          ;Mulitply the number or hexademical value of a letter by 16^place and pass it into this
          ;method again as the new 'sum.' Decrease the number the denotes the exponent by one.
          (if (number? (car hexList));if the first item in the list is a number, mulitply it by 16^place
              (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) (car hexList))))
              (if (equal? (car hexList) 'A);check if each letter has been passed in as capital or lower case
                  (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 10)))
                  (if (equal? (car hexList) 'a)
                      (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 10)))
                      (if (equal? (car hexList) 'B)
                          (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 11)))
                          (if (equal? (car hexList) 'b)
                              (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 11)))
                              (if (equal? (car hexList) 'C)
                                  (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 12)))
                                  (if (equal? (car hexList) 'c)
                                      (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 12)))
                                      (if (equal? (car hexList) 'D)
                                          (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 13)))
                                          (if (equal? (car hexList) 'd)
                                              (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 13)))
                                              (if (equal? (car hexList) 'E)
                                                  (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 14)))
                                                  (if (equal? (car hexList) 'e)
                                                      (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 14)))
                                                      (if (equal? (car hexList) 'F)
                                                          (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 15)))
                                                          (if (equal? (car hexList) 'f)
                                                              (hex2decASSIST (cdr hexList) (- place 1) (+ sum (* (expt 16 place) 15)))
                                                              sum))))))))))))))))                                         

; ********************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (dec2hex n)

; that takes a positive integer as input, and returns 
; a list of hexadecimal digits
; The following identity should hold:

;; (hex2dec (dec2hex n)) == n

; Examples

; (dec2hex 255) => '(F F)
; (dec2hex 10) => '(A)
; (dec2hex 256) => '(1 0 0)
; (dec2hex 3735928559) => '(D E A D B E E F)

; ********************************************************

(define (dec2hex n)
  (dec2hexASSIST n (list)))

(define (dec2hexASSIST decNum hexList);takes a decimal number and empty list
  (if (equal? decNum 0);if the decimal number is 0, return this hexadecimal number
      hexList
      ;Find the next hexadecimal digit by taking the remainder of the decimal number and 16
      ;Append the hexadecimal number/letter to the front of hexlist,
      ;and pass the quotient of the decNum and 16, and this updated list of hexadecimal letters into the method again
      (if (< (remainder decNum 16) 10)
          (dec2hexASSIST (quotient decNum 16) (append (list(remainder decNum 16)) hexList))
          (if (equal? (remainder decNum 16) 10)
              (dec2hexASSIST (quotient decNum 16) (append '(A) hexList))
              (if (equal? (remainder decNum 16) 11)
                  (dec2hexASSIST (quotient decNum 16) (append '(B) hexList))
                  (if (equal? (remainder decNum 16) 12)
                      (dec2hexASSIST (quotient decNum 16) (append '(C) hexList))
                      (if (equal? (remainder decNum 16) 13)
                          (dec2hexASSIST (quotient decNum 16) (append '(D) hexList))
                          (if (equal? (remainder decNum 16) 14)
                              (dec2hexASSIST (quotient decNum 16) (append '(E) hexList))
                              (dec2hexASSIST (quotient decNum 16) (append '(F) hexList))))))))))

; ********************************************************
; ** problem 5 ** (10 points)
; Write a procedure

; (range num start diff)

; that generates a list of num elements, starting with
; start and incrementing by diff

; Examples

; (range 10 1 1) => '(1 2 3 4 5 6 7 8 9 10)
; (range 3 12 -6) => '(12 6 0)
; (range 0 1 1) => '()
; (range 10 20 -2) => '(20 18 16 14 12 10 8 6 4 2)

; ********************************************************

(define (range num start diff)
  (rangeASSIST num start diff (list))
  )

(define (rangeASSIST numDigits currNum difference rlist);takes the starting parameters of range but also an empty list where the newly calculated numbers can be put
  ; the function runs until range hits 0
  (if (equal? numDigits 0)
      rlist
      (rangeASSIST (- numDigits 1) (+ currNum difference) difference (append rlist (list currNum))))
  )


; ********************************************************
; ** problem 6 ** (10 points)
; Write a procedure

; (remove-p lst pred)

; that takes a list lst and returns that list minus 
; any elements that satisfy the given predicate pred.

; Examples:

; (remove-p '(1 2 3 4 5 6) even?) => '(1 3 5)
; (remove-p '(1 2 3 4 5 6) odd?) => '(2 4 6)
; (remove-p '(1 2 3 4 5 6) (lambda (a) (> a 3))) => '(1 2 3)
; (remove-p '(1 2 3 4 5 6) (lambda (a) (< a 3))) => '(3 4 5 6)

; ********************************************************


(define (remove-p lst pred)
  (removeASSIST lst pred (list)))

(define (removeASSIST startList pred newList);takes the same parameters as remove-p, but adds an
  ;empty list, so that as the method cycles through the given list, it can put any variables that
  ;fail the predicate into the empty list and return this new list
  (if (equal? (length startList) 0)
      newList
      (if (pred (car startList))
          (removeASSIST (cdr startList) pred newList)
          (removeASSIST (cdr startList) pred (append newList (list(car startList))))
          )
      )
  )

; ********************************************************
; ** problem 7 ** (10 points)
; Write a procedure 

; (collect-p lst pred)

; that takes a list lst and returns that list containing only
; the elements that satisfy the given predicate pred.

; Examples:

; (collect-p '(1 2 3 4 5 6) even?) => '(2 4 6)
; (collect-p '(1 2 3 4 5 6) odd?) => '(1 3 5)
; (collect-p '(1 2 3 4 5 6) (lambda (a) (> a 3))) => '(4 5 6)
; (collect-p '(1 2 3 4 5 6) (lambda (a) (< a 3))) => '(1 2)


(define (collect-p lst pred)
  (collectASSIST lst pred (list)))

(define (collectASSIST startList pred newList);takes the same parameters as collect-p, but adds an
  ;empty list, so that as the method cycles through the given list, it can put any variables that
  ;fit the predicate into the empty list and return this new list
  (if (equal? (length startList) 0)
      newList
      (if (pred (car startList))
          (collectASSIST (cdr startList) pred (append newList (list(car startList))))
          (collectASSIST (cdr startList) pred newList))))


; ********************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (starts-with? lst1 lst2)
; (contains? lst1 lst2)

; starts-with? returns #t if lst1 matches the beginning
; of lst2.  That is, if by deleting elements off the end of
; lst2, you can end up with lst1.

; contains? returns #t if lst1 is inside lst2.  That is, if
; by deleting elements from either or both ends of lst2,
; you can end up with lst1.

; Examples:

; (starts-with? '(1 2 3) '(1 2 3 4 5)) => #t
; (starts-with? '(1 2 3 4 5) '(1 2 3)) => #f
; (starts-with? '() '(1 2 3)) => #t
; (starts-with? '(1) '(1)) => #t

; (contains? '(3 4 5) '(1 2 3 4 5 6)) => #t
; (contains? '() '(1 2 3)) => #t
; (contains? '(4 5 6) '(1 2 3 4 5)) => #f
; (contains? '(5) '(1 2 3 4 5 6)) => #t

; ********************************************************


(define (starts-with? testing main)
  (if (equal? (length testing) 0)
      #t
      (if (equal? (length main) 0)
          #f
          (if (equal? (car testing) (car main))
              (starts-with? (cdr testing) (cdr main))
              #f)))) 


(define (contains? testing main)
  (if (equal?(length testing) 0)
      #t
      (if (empty? main)
          #f
          (if (equal? (car testing) (car main))
              (if (containsASSIST2 (cdr testing) (cdr main))
                  #t
                  (contains? testing (cdr main)))
              (contains? testing (cdr main))))))

(define (containsASSIST2 char main);takes this first item in the first list and all of
  ;the second list and checks if the item equals the first item in the main list. If they are not equal,
  ;it passes the item and the cdr of the list back into the method
  ;(display char)
  ;(display main)
  (if (equal? (length char) 0)
      #t
      (if (equal? (length main) 0)
          #f
          (if (equal? (car char) (car main))
              (containsASSIST2 (cdr char) (cdr main))
              #f))))

; ********************************************************
; ** problem 9 (10 points)
; Write 

; (power-set lst)

; which treats the lst as a set and returns a list of all possible
; subsets

; Examples:
; (power-set '()) => '(())
; (power-set '(1)) => '(() (1))
; (power-set '(1 2)) => '(() (2) (1) (1 2))
; (power-set '(1 2 3)) => '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
; (power-set '(1 2 3 4)) => 
; '(() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4) (1) (1 4) (1 3) (1 3 4) 
;   (1 2) (1 2 4) (1 2 3) (1 2 3 4))

; (define toppings '(onion peppers bacon sausage mushroom))
; (power-set toppings)
; '(() (mushroom) (sausage) (sausage mushroom) (bacon) (bacon mushroom)
; (bacon sausage) (bacon sausage mushroom) (peppers) (peppers mushroom)
; (peppers sausage) (peppers sausage mushroom) (peppers bacon) (peppers
; bacon mushroom) (peppers bacon sausage) (peppers bacon sausage
; mushroom) (onion) (onion mushroom) (onion sausage) (onion sausage
; mushroom) (onion bacon) (onion bacon mushroom) (onion bacon sausage)
; (onion bacon sausage mushroom) (onion peppers) (onion peppers
; mushroom) (onion peppers sausage) (onion peppers sausage mushroom)
; (onion peppers bacon) (onion peppers bacon mushroom) (onion peppers
; bacon sausage) (onion peppers bacon sausage mushroom))



(define (power-set lst)
  (recurse lst '(())))

(define (recurse startList finalList);takes the given list and an new list.
  ;If the given list isn't empty, it passes the cdr of the first list and the
  ;list generated by calling 'add' on the car of the given and the entirety
  ;of the new list into the method again
  (if (equal? (length startList) 0)
      finalList
      (recurse (cdr startList) (add (car startList) finalList '()))))

(define (add item finalList newList);This method takes an item and two lists. It stops running when the first list is empty.
  ;It adds the item onto every item in the first list and places these lists into the second list.
  (if (equal? (length finalList) 0)
      newList
      (add item
           (cdr finalList)
           (append (list(car finalList)) (append newList (list(cons item (car finalList))))))))
           
; ********************************************************
; ** problem 10 (10 points)
; 
; You want to automate your binge watching of movies.
; You do not care about quality of the movies, just quantity.
; Given a list of movie start and end times, figure out 
; the largest number of movies you can watch in their entirety.
; You may assume that if movie A ends at time N and movie B
; starts at time N, you can watch both A and B.

; Write

; (most-movies lst)

; where lst is a list of movie (starttime endtime) pairs.
; most-movies will return a number indicating the most movies
; you can watch from that list.


; Examples:

; (most-movies '((1 2) (2 3)) => 2
; (most-movies '((1 2) (2 3) (3 4) (1 5))) => 3
; (most-movies '((3 4) (1 2) (1 5) (4 5) (0 1)) => 4

; There are many ways to approach this problem.  I have primed you
; by giving you the power-set problem above, which can be used
; in a brute force approach.

; However, for long lists, the brute force approach can run out of 
; memory and time. There is a very nice solution that is made
; even more efficient using memoization. 
; I'll award an extra 5 points to anyone who comes up with a
; solution more efficient than the brute force approach.


; ********************************************************

(define (most-movies lst)
  (if (equal? (power-set lst) 0)
      0
      (movieHelpOne (power-set lst) 1)))

(define (movieHelpOne allMovieCombos mostSoFar);takes the powerset and a variable marking the most movies that
  ;have been watched thus far. Calls movieHelpTwo. If mHT says more movies could have been watched, it remebers that new number and
  ;takes the cdr of the power-set
  (if (empty? allMovieCombos);gone through all movies
      mostSoFar
      (if (empty? (car allMovieCombos));the first item in the list is empty
          (movieHelpOne (cdr allMovieCombos) mostSoFar);skip it and go on
          (if (equal? (length (car allMovieCombos))1);is the first item a length of one
              (movieHelpOne (cdr allMovieCombos) mostSoFar)
              (let ([tempMost (movieHelpTwo (car allMovieCombos))]);take the first item & send it to #2
                (if (> tempMost mostSoFar)
                    (movieHelpOne (cdr allMovieCombos) tempMost)
                    (movieHelpOne (cdr allMovieCombos) mostSoFar)))))))

(define (movieHelpTwo thisList);takes one list in power-set
  (if (movieHelpThree thisList);pass the list into new method
      (length thisList);return the length of the list
      1));otherwise return one

(define (movieHelpThree thisList);takes a list from power-set and passes the cdr of the
  ;list and the first item in the first list and the second item in the first list into movieHelpFour.
  ;It cycles through the whole list.
  (if (or (equal? (length thisList) 0)(equal? (length thisList) 1))
      #t
      (if (movieHelpFour (cdr thisList) (car (car thisList)) (second (car thisList)))
          (movieHelpThree (cdr thisList))
          #f)))

(define (movieHelpFour thisList start end);checks if any items in the list conflict with the start and
  ;end times of the first movie. Returns true if they don't and flase if they do.
  (if (equal? (length thisList) 0)
      #t
      (if (or (>= (caar thisList) end)(<= (second (car thisList)) start))
          (movieHelpFour (cdr thisList) start end)
          #f)))



; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (test name got expected)
  (cond (*testing-flag*
         (let* ((expected (if (procedure? expected)
                              (and (expected got) 'OK-TEST)
                              expected))
                (prefix (if (equal? got expected)
                            'OK
                            'X)))
           (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))
	
(test 'bin2dec (bin2dec '(1 0 1 0)) 10)
(test 'bin2dec (bin2dec '(1 1 1 1 1)) 31)
(test 'bin2dec (bin2dec '(1 1 1 1 1 1 1 0 0 0 0 1)) 4065)

(test 'dec2bin (dec2bin 10) '(1 0 1 0))
(test 'dec2bin (dec2bin 31) '(1 1 1 1 1))
(test 'dec2bin (dec2bin 4065) '(1 1 1 1 1 1 1 0 0 0 0 1))

(test 'hex2dec (hex2dec '(A)) 10)
(test 'hex2dec (hex2dec '(1)) 1)
(test 'hex2dec (hex2dec '(F F)) 255)
(test 'hex2dec (hex2dec '(1 0 0)) 256)
(test 'hex2dec (hex2dec '(d e a d b e e f)) 3735928559)
(test 'hex2dec (hex2dec '(f f f f f f f f f f f f)) 281474976710655)
 
(test 'dec2dex (dec2hex 10) '(A))
(test 'dec2dex (dec2hex 7) '(7))
(test 'dec2hex (dec2hex 255) '(F F))
(test 'dec2hex (dec2hex 256) '(1 0 0))
(test 'dec2hex (dec2hex 3735928559) '(D E A D B E E F))
(test 'hex2dec (dec2hex 281474976710655) '(F F F F F F F F F F F F))

(test 'range (range 10 1 1) '(1 2 3 4 5 6 7 8 9 10))
(test 'range (range 0 1 1) '())
(test 'range (range 0 0 0) '())
(test 'range (range 3 12 -6) '(12 6 0))
(test 'range (range 0 1 1) '())
(test 'range (range 10 20 -2) '(20 18 16 14 12 10 8 6 4 2))

(test 'remove-p (remove-p '(1 2 3 4 5 6) even?) '(1 3 5))
(test 'remove-p (remove-p '() even?) '())
(test 'remove-p (remove-p '(1 2 3 4 5 6) odd?) '(2 4 6))
(test 'remove-p (remove-p '(1 2 3 4 5 6) (lambda (a) (> a 3))) '(1 2 3))
(test 'remove-p (remove-p '(1 2 3 4 5 6) (lambda (a) (< a 3))) '(3 4 5 6))

(test 'collect-p (collect-p '(1 2 3 4 5 6) even?) '(2 4 6))
(test 'collect-p (collect-p '(1 2 3 4 5 6) odd?) '(1 3 5))
(test 'collect-p (collect-p '() even?) '())
(test 'collect-p (collect-p '(1 2 3 4 5 6) (lambda (a) (> a 3))) '(4 5 6))
(test 'collect-p (collect-p '(1 2 3 4 5 6) (lambda (a) (< a 3))) '(1 2))


(test 'starts-with? (starts-with? '(1 2 3) '(1 2 3 4 5)) #t)
(test 'starts-with? (starts-with? '(1 2 3 4 5) '(1 2 3)) #f)
(test 'starts-with? (starts-with? '() '(1 2 3)) #t)
(test 'starts-with? (starts-with? '(1) '(1)) #t)
(test 'starts-with? (starts-with? '() '()) #t)
(test 'starts-with? (starts-with? '(2 1) '(1)) #f)

(test 'contains? (contains? '(3 4 5) '(1 2 3 4 5 6)) #t)
(test 'contains? (contains? '() '(1 2 3)) #t)
(test 'contains? (contains? '(3) '(1 2 3)) #t)
(test 'contains? (contains? '(10) '(1 2 3 4 5 6 7 8 9 10)) #t)
(test 'contains? (contains? '(4 5 6) '(1 2 3 4 5)) #f)
(test 'contains? (contains? '(5) '(1 2 3 4 5 6)) #t)
(test 'contains? (contains? '(3 5) '(1 2 3 4 5 6)) #f)
(test 'contains? (contains? '(6 5) '(1 2 3 4 5 6)) #f)
(test 'contains? (contains? '(1 5) '(1 2 3 4 5 6)) #f)
(test 'contains? (contains? '(5 5) '(1 2 3 4 5 6)) #f)

(test 'power-set (power-set '()) '(()))
(test 'power-set (power-set '(1)) '(() (1)))
(test 'power-set (power-set '(1 2)) '(() (2) (1) (1 2)))
(test 'power-set (power-set '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
;(test 'power-set (power-set '(onion peppers bacon sausage mushroom)) '(() (mushroom) (sausage) (sausage mushroom) (bacon) (bacon mushroom) (bacon sausage) (bacon sausage mushroom) (peppers) (peppers mushroom) (peppers sausage) (peppers sausage mushroom) (peppers bacon) (peppers bacon mushroom) (peppers bacon sausage) (peppers bacon sausage mushroom) (onion) (onion mushroom) (onion sausage) (onion sausage mushroom) (onion bacon) (onion bacon mushroom) (onion bacon sausage) (onion bacon sausage mushroom) (onion peppers) (onion peppers mushroom) (onion peppers sausage) (onion peppers sausage mushroom) (onion peppers bacon) (onion peppers bacon mushroom) (onion peppers bacon sausage) (onion peppers bacon sausage mushroom)))

(test 'most-movies (most-movies '((1 2) (2 3))) 2)
(test 'most-movies (most-movies '((1 2) (2 3) (3 4) (1 5))) 3)
(test 'most-movies (most-movies '((3 4) (1 2) (1 5) (4 5) (0 1))) 4)
(test 'most-movies (most-movies '((3 4) (1 2) (1 5) (4 5) (0 1) (5 6) (6 7) (7 8) (6 8) (1 8) (3 5) (8 9))) 8)

;*********************************************************
;***** end of hw #1