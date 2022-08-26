(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))


#+#:excluded (loop repeat 10 do
                   (format t "~5t~a ~15t~a ~25t~a~%"
                           (random-animal)
                           (random-animal)
                           (random-animal)))
;      dog       kangaroo  walrus
;      dog       tiger     kangaroo
;      tiger     tick      tiger
;      kangaroo  tiger     tick
;      kangaroo  kangaroo  walrus
;      tick      tick      dog
;      dog       kangaroo  walrus
;      dog       walrus    dog
;      kangaroo  tiger     tick
;      walrus    tick      tick


#+#:excluded (loop repeat 10 do
                   (format t "~30<~a~;~a~;~a~>~%"
                           (random-animal)
                           (random-animal)
                           (random-animal)))
; tiger     walrus      kangaroo
; tiger        tick        tiger
; tiger       dog       kangaroo
; tick       kangaroo        dog
; walrus        tiger        dog
; tiger     kangaroo      walrus
; kangaroo    walrus    kangaroo
; tick      kangaroo      walrus
; walrus     kangaroo      tiger
; kangaroo    walrus    kangaroo


#+#:excluded (loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))
;             tiger             
;            kangaroo           
;              dog              
;              tick             
;             walrus            
;             walrus            
;              dog              
;              tick             
;             tiger             
;             tiger             


#+#:excluded (loop repeat 10 do
                   (format t "~30:@<~a~;~a~;~a~>~%"
                           (random-animal)
                           (random-animal)
                           (random-animal)))
;      dog     dog     dog      
;    tick   kangaroo   tiger    
;   kangaroo  tiger  kangaroo   
;    tiger   dog    kangaroo    
;     tick    dog     tiger     
;    dog   kangaroo   walrus    
;     dog    dog     walrus     
;     dog     tiger     dog     
;   tick  kangaroo   kangaroo   
;     dog    tiger    tiger     


#+#:excluded (loop repeat 10 do
                   (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
                           (random-animal)
                           (random-animal)
                           (random-animal)))
;   walrus     dog       dog    
;    tick     walrus     dog    
;   walrus     dog      walrus  
;    dog       dog      walrus  
;  kangaroo    tick     tiger   
;  kangaroo   tiger      tick   
;  kangaroo   walrus     dog    
;    dog       tick      tick   
;    dog      tiger      dog    
;  kangaroo   walrus    walrus  



#+#:excluded (format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
; | 0  1  2  3  4  5  6  7  8  9 |
; |10 11 12 13 14 15 16 17 18 19 |
; |20 21 22 23 24 25 26 27 28 29 |
; |30 31 32 33 34 35 36 37 38 39 |
; |40 41 42 43 44 45 46 47 48 49 |
; |50 51 52 53 54 55 56 57 58 59 |
; |60 61 62 63 64 65 66 67 68 69 |
; |70 71 72 73 74 75 76 77 78 79 |
; |80 81 82 83 84 85 86 87 88 89 |
; |90 91 92 93 94 95 96 97 98 99 |
;
; To create this nicely formatted table of numbers, we firsts use the looping
; control sequences ~{ ~} to iretare through a list of numbers created by the
; loop command.  Within the iteration, we place justification control sequences
; ~< ~>, which we've used earlier.  In this case, we don't use them to justify our
; text, but instead use them to divide the resulting text into pieces.  This is how
; we break our 100 numbers into nice clean rows of 10.  We place the ~:; control
; sequence inside our justification control sequences ~< ~>, which causes text to
; be broken into pieces of equal length.
;
; When used insidxce a justfication, the control string proceding this sequence
; ~:; (which in this case happens to be |~%|) will be triggered only if the current
; cursor position is beyond a certain point, as specified by the second parameter,
; 33.  In other words, we're telling the format function "Hey, once you have 33
; characters' worth of text, start a fresh line."
;
; The |~%| control string causes the line break and vertical bars to be printed.
; The number to be displayed is formatted using ~2d, which prints a left-justified
; number, two characters wide.
; 
; Note: full details on every single control sequence, see the Common Lisp HyperSpect at
; http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm


(defun robots ()
  (loop named main
        ; These are the 8 offsets for when the game board is 64 wide
        with directions = '((q . -65) (w . -64) (e . -63)
                            (a .  -1)           (d .   1)
                            (z .  63) (x .  64) (c .  65))
        for pos = 544
        then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                    (force-output)
                    (let* ((c (read))
                           (d (assoc c directions)))
                      (cond (d (+ (cdr d) pos))
                            ; The game board is 64x64=1024
                            ((eq c 't) (random 1024))
                            ((eq c 'l) (return-from main 'bye))
                            (t pos))))
        for monsters = (loop repeat 10 collect (random 1024))
        then (loop for mpos in monsters
                   collect (if (> (count mpos monsters) 1)
                             mpos
                             (cdar (sort (loop for (k . d) in directions
                                               for new-mpos = (+ mpos d)
                                               collect (cons (+ (abs (- (mod new-mpos 64)
                                                                        (mod pos 64)))
                                                                (abs (- (ash new-mpos -6)
                                                                        (ash pos -6))))
                                                             new-mpos))
                                         '<
                                         :key #'car))))
        ; Check if monster as scrap!
        when (loop for mpos in monsters
                   always (> (count mpos monsters) 1)) return 'player-wins
        do (format t
                   "~%|~{~<|~%|~,65:;~A~>~}|"
                   (loop for p below 1024
                         collect (cond ((member p monsters)
                                        (cond ((= p pos) (return-from main 'player-loses))
                                              ; Collided robots - scrap
                                              ((> (count p monsters) 1) #\#)
                                              ; Robot
                                              (t #\A)))
                                        ((= p pos) #\@)
                                        (t #\Space))))))

  #+#:excluded (robots)
