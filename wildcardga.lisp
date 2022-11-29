; Global variable for the # of atoms in pitch-string
( setf *limit* 26 )

; Only supports C major key...could add more keys in the future
( setf *CMAJOR* '(C D E F G A B C2 D2 E2 F2 G2 A2 B2 
                        C/2 D/2 E/2 F/2 G/2 A/2 B/2) )

; Global var for adding more keys in the future
( setf *CURRENT-KEY* *CMAJOR* )

; Function that returns a random pitch in the specified pitch-list
( defmethod derive-pitch ()
    ; C major + durations quarter note, half-note, and eighth note
    ( setf pitch-list *CMAJOR* )
    ( nth ( random ( length pitch-list ) ) pitch-list )
)

; Method that creates pitch-list
( defmethod pitch-string ()
    ( pitch-string-helper *limit* )
)

; Recursive function that creates pitch-list using parameter n = list size
( defmethod pitch-string-helper ( n )
    (cond
        (( = n 0 )
            '()
        )
        ( t
            ( cons ( derive-pitch ) ( pitch-string-helper ( - n 1 ) ) )
        )
    )
)

; Method that mutates a given list
( defmethod mutation ( ( pitch-str list ) &aux position symbol )
    ( setf position ( random ( length pitch-str ) ) )
    ( setf symbol ( others *CURRENT-KEY* ( nth position pitch-str ) ) )
    ( change pitch-str ( pick symbol ) position )
)

; Recursive method that returns list of pitches from *CURRENT-KEY* excluding
; the parameter atom
( defmethod others ( ( li list ) pitch )
    ( cond
        (( null li )
            '()
        )
        (( not ( equal pitch ( car li ) ) )
            ( cons ( car li ) ( others ( cdr li ) pitch ) )
        )
        (t
            ( others ( cdr li ) pitch )
        )
    )
)

; Method to pick a random element form a list
( defmethod pick ( ( symbols list ) ) 
    ( nth ( random ( length symbols ) ) symbols )
)

; Method that changes an element of a list at a specific position
; to a give symbol
( defmethod change ( ( str list ) symbol pos )
    ( cond
        (( = pos 0 )
            ( append ( list symbol ) ( cdr str ) )
        )
        (t
            ( cons ( car str ) ( change ( cdr str ) symbol ( - pos 1 ) ) )
        )
    )
)

; Method that appends pieces of two lists together based on a pivot
( defmethod crossover ( ( m list ) ( f list ) &aux pos )
    ( setf pos ( + 1 ( random ( length m ) ) ) )
    ( append ( first-n m pos ) ( rest-n f pos ) )
)

; Recursive method that outputs the first n elements of a list as a list
( defmethod first-n ( ( m list ) pos )
    ( cond
        (( = pos 0 )
            '()
        )
        (t
            ( cons ( car m ) ( first-n ( cdr m ) ( - pos 1 ) ) )
        )
    )
)

; Recursive method that outputs the remaining n elements of a list as a list
( defmethod rest-n ( ( f list  ) pos )
    (cond
        (( = pos 0 )
            f
        )
        (t
            ( rest-n ( cdr f ) ( - pos 1 ) )
        )
    )
)

; Method to demo the mutation method
( defmethod mutation-demo (&aux s m)
    ( setf s ( pitch-string ) )
    ( dotimes ( i 10 )
        ( format t "s = ~A~%" s )
        ( setf m ( mutation s ) )
        ( format t "m = ~A~%~%" m )
    )
)

; Method to demo the crossover method
( defmethod crossover-demo (&aux m f x)
    ( setf m ( pitch-string ) )
    ( setf f ( pitch-string ) )
    ( dotimes ( i 10 )
        ( format t "m = ~A~%" m )
        ( setf x ( crossover m f ) )
        ( format t "x = ~A~%" x )
        ( format t "f = ~A~%~%" f )
    )
)

; Fitness metric for "stepwise motion" in a melody
;    -If pitches are 1 jump away from each other add 1
;    -If pitches are 2 jumps away from each other add 0.5
;    -If pitches are 0 jumps away from each other add 0.8
;    -Otherwise, add nothing
( defmethod fitness-stepwise-motion ( ( li list ) )
    ; use mod to account for duplicate notes with different durations
    ( cond
        (( > (length li ) 1 )
            ( setf first-pitch-int ( mod ( position ( car li ) *CURRENT-KEY* ) 7 ) )
            ( setf second-pitch-int ( mod ( position ( cadr li ) *CURRENT-KEY* ) 7 ) )
            ( setf pitch-distance ( abs ( - first-pitch-int second-pitch-int ) ) )
        )
    )
    (cond
        (( = ( length li ) 1 )
            0
        )
        (( = pitch-distance 1 )
            ( + 1 ( fitness-stepwise-motion ( cdr li ) ) )
        )
        (( = pitch-distance 2 )
            ( + 0.5 ( fitness-stepwise-motion ( cdr li ) ) )
        )
        (( = pitch-distance 0 )
            ( + 0.8 ( fitness-stepwise-motion ( cdr li ) ) )
        )
        (t
            ( fitness-stepwise-motion ( cdr li ) )
        )

    )

)

; Fitness method that favors pairs of notes
;    -if the length of the list is odd: reward +1 for similar notes
;    -if the length of the list is even: reward +1 for different notes 
( defmethod fitness-pairs ( ( li list ) )
    ( cond 
        (( > ( length li ) 1)
            ( setf first-pitch-int ( mod ( position ( car li ) *CURRENT-KEY* ) 7 ) )
            ( setf second-pitch-int ( mod ( position ( cadr li ) *CURRENT-KEY* ) 7 ) )
            ( setf pitch-distance ( abs ( - first-pitch-int second-pitch-int ) ) )
        )
    )
    (cond
        (( = ( length li ) 1 )
            0
        )
        ; odd length = different note pair, even length = same note pair
        (( and ( = ( mod ( length li ) 2 ) 1 ) ( > pitch-distance 0 ) )
                ( + 1 ( fitness-pairs ( cdr li ) ) )
        )
        (( and ( = ( mod ( length li ) 2 ) 0 ) ( = pitch-distance 0 ) )
            ( + 1 ( fitness-pairs ( cdr li ) ) )
        )
        (t
            ( + 0 ( fitness-pairs ( cdr li ) ) )
        )
    )
)

; Fitness method that favors melodies with ascending jumps and
; descending lines of two or three or four notes
;    -award +1 point for pitch-distance greater than 1
;    -award +1 point for pitch-distance equal to -1
( defmethod fitness-ascending-jumps ( ( li list ) )
    (cond
        (( > ( length li ) 1 )
            ( setf first-pitch-int ( mod ( position ( car li ) *CURRENT-KEY* ) 7 ) )
            ( setf second-pitch-int ( mod ( position ( cadr li ) *CURRENT-KEY* ) 7 ) )
            ( setf pitch-distance ( - first-pitch-int second-pitch-int ) )
        )
    )
    (cond
        (( = ( length li ) 1 )
            0
        )
        (( > pitch-distance 1 )
            ( + 1 ( fitness-ascending-jumps ( cdr li ) ) )
        )
        (( = pitch-distance -1 )
            ( + 1 ( fitness-ascending-jumps ( cdr li ) ) )
        )
        (t
            (+ 0 ( fitness-ascending-jumps ( cdr li ) ) )
        )
    )
)

; Fitness method that favors slashing melodies consisting of ascending and descending lines of two
; or three or four notes
( defmethod fitness-stepwise-slashing ( ( li list ) )
    ( fitness-slashing-helper li 0 0 )
)

; Helper recursion method that favors slashing melodies consisting of ascending and descending lines of two
; or three or four notes -- prev-pitch-dist stores pitch distance of previous note pair
; -+1 if stepwise for 2, 3, or 4 notes in a row

( defmethod fitness-slashing-helper ( ( li list ) prev-pitch-dist notes-num )
    ( cond 
        (( > ( length li ) 1 )
            ( setf first-pitch-int ( mod ( position ( car li ) *CURRENT-KEY* ) 7 ) )
            ( setf second-pitch-int ( mod ( position ( cadr li ) *CURRENT-KEY* ) 7 ) )
            ( setf pitch-distance ( - first-pitch-int second-pitch-int ) )
        )
    )
    ( cond 
        (( = ( length li ) 1 )
            0
        )
        (   ( and 
                ( > notes-num 0 )
                ( < notes-num 3 )
                ( fitness-ascending-descending-p prev-pitch-dist pitch-distance )
            )
            ( + 1 ( fitness-slashing-helper ( cdr li ) pitch-distance ( + notes-num 1 ) ) )
        )
        ; check if first two notes are stepwise, if so, reward +1
        ( ( and 
                ( or ( = pitch-distance 1 ) ( = pitch-distance -1 ) )
                ( = notes-num 0 )
          )
            ( + 1 ( fitness-slashing-helper ( cdr li ) pitch-distance ( + notes-num 1 ) ) )
        )
        (t
            ( fitness-slashing-helper ( cdr li ) pitch-distance 0 )
        )
    )

)

; Predicate used to determine if notes are stepwise ascending/descending compared to a previous
; direction
( defmethod fitness-ascending-descending-p ( prev-direction pitch-distance )
        ( or 
            ( and ( = pitch-distance 1) ( = prev-direction 1 ) ) 
            ( and ( = pitch-distance -1 ) ( = prev-direction -1 ) )            
        )
)

; Fitness method used to favor melodies which zig and zag but only turn on certain notes ( degrees 
; 1, 3, 5, and 7 of the scale ), with some repeated notes 
( defmethod fitness-zig-zag ( ( li list ) )
    ( fitness-zig-zag-helper li 0 )
)
; Recursive helper method method for fitness-zig-zag
;   -+1 point is rewarded for turning (stepwise) on degree 1, 3, 5, or 7
;   -+0.6 is rewarded for stepwise motion
;   -+0.3 is rewarded for repeated notes
( defmethod fitness-zig-zag-helper ( ( li list ) prev-pitch-dist )
    (cond 
        (( > ( length li ) 1 )
            ( setf first-pitch-int ( mod ( position ( car li ) *CURRENT-KEY* ) 7 ) )
            ( setf second-pitch-int ( mod ( position ( cadr li ) *CURRENT-KEY* ) 7 ) )
            ( setf pitch-distance ( - first-pitch-int second-pitch-int ) )
        )
    )
    (cond 
        (( = ( length li ) 1 )
            0
        )
        (   (and 
                ( fitness-zig-zag-p prev-pitch-dist pitch-distance )
                ( or 
                    ( = ( + first-pitch-int 1 ) 1 )
                    ( = ( + first-pitch-int 1 ) 3 )
                    ( = ( + first-pitch-int 1 ) 5 )
                    ( = ( + first-pitch-int 1 ) 7 )
                )
            )
                ( + 1 ( fitness-zig-zag-helper ( cdr li ) pitch-distance ) )
        )
        (( or ( = pitch-distance 1 ) ( = pitch-distance -1 ) )
            ( + 0.6 ( fitness-zig-zag-helper ( cdr li ) pitch-distance ) )
        )
        ((= pitch-distance 0 )
            ( + 0.3 ( fitness-zig-zag-helper ( cdr li ) pitch-distance ) )
        )
        (t
            ( fitness-zig-zag-helper ( cdr li ) pitch-distance  )
        )

    )
)

; Predicate function to check if a note pair "zig zags" based on note before
( defmethod fitness-zig-zag-p ( prev-direction pitch-distance )
    ( or 
        ( and ( = pitch-distance 1 ) ( < prev-direction 0 ) )
        ( and ( = pitch-distance -1 ) ( > prev-direction 0 ) )
    )
)

; Method to test the five fitness methods
( defmethod fitness-demo (&aux x fitness)
    ( setf x ( pitch-string ) )
    ( format t "x = ~A~%" x )
    ( format t "Directly applying the fitness metrics ...~%" )
    ( format t "fitness-stepwise-motion = ~A~%" ( fitness-stepwise-motion x ) )
    ( format t "fitness-pairs = ~A~%" ( fitness-pairs x ) )
    ( format t "fitness-ascending-jumps = ~A~%" ( fitness-ascending-jumps x ) )
    ( format t "fitness-stepwise-slashing = ~A~%" ( fitness-stepwise-slashing x ) )
    ( format t "fitness-zig-zag = ~A~%" ( fitness-zig-zag x ) )
    ( format t "Indirectly applying the fitness metrics ... ~%" )
    ( setf fitness #'fitness-stepwise-motion )
    ( format t "fitness-stepwise-motion = ~A~%" ( funcall fitness x ) )
    ( setf fitness #'fitness-pairs )
    ( format t "fitness-pairs = ~A~%" ( funcall fitness x ) )
    ( setf fitness #'fitness-ascending-jumps )
    ( format t "fitness-ascending-jumps = ~A~%" ( funcall fitness x ) )
    ( setf fitness #'fitness-stepwise-slashing )
    ( format t "fitness-stepwise-slashing = ~A~%" ( funcall fitness x ) )
    ( setf fitness #'fitness-zig-zag )
    ( format t "fitness-zig-zag = ~A~%" ( funcall fitness x ) )
)

( defclass individual () 
    (
        ( melody :accessor individual-melody :initarg :melody )
        ( fitness :accessor individual-fitness :initarg :fitness )
        ( number :accessor individual-number :initarg :number )
    )
)

( defmethod random-individual (&aux melody )
    ( setf melody ( pitch-string ) )
    ( make-instance 'individual 
        :melody melody
        :fitness ( funcall *fitness* melody )
        :number 0
    )
)

( defmethod new-individual ( ( nr number ) ( notes list ) )
    ( make-instance 'individual 
        :melody notes 
        :fitness ( funcall *fitness* notes )
        :number nr 
    )
)

( defmethod display ( ( i individual ) )
    ( display-nnl i ) ( terpri )
)

( defmethod display-nnl ( ( i individual ) )
    ( prin1 ( individual-number i ) )
    ( princ ( filler ( individual-number i ) ) )
    ( prin1 ( individual-melody i ) )
    ( princ "  " )
    ( prin1 ( individual-fitness i ) )
    ( princ ( filler ( individual-fitness i ) ) )
)

( defmethod filler ( ( n number ) )
    ( cond
        ( ( < n 10 ) "     " )
        ( ( < n 100 ) "    " )
        ( ( < n 1000 ) "   " )
        ( ( < n 10000 ) "  " )
        ( ( < n 100000 ) " ")
    )
)

( defmethod fitness-stepwise-motion ( ( i individual ) )
    ( fitness-stepwise-motion ( individual-melody i ) )
)

( defmethod fitness-pairs ( ( i individual ) )
    ( fitness-pairs ( individual-melody i ) )
)

( defmethod fitness-ascending-jumps ( ( i individual ) )
    ( fitness-ascending-jumps ( individual-melody i ) )
)

; Demo for individual class methods
( defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
    ( setf *fitness* #'fitness-stepwise-motion )
    ( setf i0 ( random-individual ) )
    ( display i0 )
    ( setf one ( pitch-string  ) )
    ( setf i1 ( new-individual 1 one ) )
    ( display i1 )
    ( setf two ( pitch-string  ) )
    ( setf i2 ( new-individual 2 two ) )
    ( display i2 )
    ( setf three ( pitch-string ) )
    ( setf i3 ( new-individual 3 three ) )
    ( display i3 )
    ( format t "Fitness of i0 = ~A~%" ( funcall *fitness* i0 ) )
    ( format t "Fitness of i1 = ~A~%" ( funcall *fitness* i1 ) )
    ( format t "Fitness of i2 = ~A~%" ( funcall *fitness* i2 ) )
    ( format t "Fitness of i3 = ~A~%" ( funcall *fitness* i3 ) )
    nil
)

( defconstant *population-size* 100 )
( defconstant *selection-size* 8 )
( setf *fitness* #'fitness-stepwise-motion )
( defclass population ()
    (
        ( individuals :accessor population-individuals :initarg :individuals )
        ( generation :accessor population-generation :initform 0 )
    )
)

( defmethod size ( ( p population ) )
    ( length ( population-individuals p ) )
)

( defmethod display ( ( p population ) )
    ( terpri ) ( terpri )
    ( princ "Generation " )
    ( prin1 ( population-generation p ) )
    ( princ " population ... " )
    ( terpri ) ( terpri )
    ( dolist ( i ( population-individuals p ) )
        ( display i )
    )
    ( terpri )
)

( defmethod initial-population (&aux individuals )
    ( setf individuals () )
    ( dotimes ( i *population-size* )
        ( push ( new-individual ( + i 1 ) ( pitch-string ) ) individuals )
    )
    ( make-instance 'population :individuals ( reverse individuals ) )
)

( defmethod average ( ( p population ) &aux ( sum 0 ) )
    ( setf indiv-list ( population-individuals p ) )
    ( setf sum ( sum-nums ( mapcar #'individual-fitness indiv-list ) ) )
    ( float ( / sum *population-size* ) )
)

; helper method that calculates the sum of a list of numbers
( defmethod sum-nums ( ( li list ) )
    (cond
        ((null li)
            0
        )
        (t
            ( + ( car li ) ( sum-nums ( cdr li ) ) )
        )
    )
)

( setf *select-demo*  nil )

( defmethod select-individual ( ( p population ) 
    &aux i candidates rn )
    ( setf candidates ( select-individuals p ) )
    ( setf mfi ( most-fit-individual candidates ) )
    ( if *select-demo* ( select-demo-helper candidates mfi ) )
    mfi
)

( defmethod select-individuals ( ( p population )
    &aux individuals candidates rn )
    ( setf individuals ( population-individuals p ) )
    ( setf candidates () )
    ( dotimes ( i *selection-size* )
        ( setf rn ( random *population-size* ) )
        ( push ( nth rn individuals ) candidates )
    )
    candidates
)

( defmethod most-fit-individual ( ( l list ) &aux max-value max-individual )
    ( setf max-individual ( max-val l 0 ) )
    ( setf max-value ( individual-fitness max-individual ) )
    max-individual
)

; helper method that returns the individual with maximum fitness using recursion
( defmethod max-val ( ( l list ) current-max )
    (cond
        ((null l)
            current-max
        )
        ((or (equal current-max 0 ) ( > ( individual-fitness ( car l ) ) ( individual-fitness current-max ) ) )
            ( max-val ( cdr l ) ( car l ) )
        )
        (t
            ( max-val (cdr l ) current-max )
        )
    )
)

( defmethod select-demo-helper ( ( l list ) ( i individual ) )
    ( princ "the sample of individuals ..." ) ( terpri )
    ( mapcar #'display l )
    ( terpri )
    ( princ "the most fit of the sample ... " ) ( terpri )
    ( display i )
    ( terpri )
    nil
)

( defmethod population-demo (&aux p)
    ( setf p ( initial-population ) )
    ( display p )
    ( format t "Average fitness = ~A~%~%" ( average p ) )
    ( setf *select-demo* t )
    ( format t "Sampling ... ~%~%" )
    ( select-individual p ) ( terpri )
    ( format t "Sampling ... ~%~%" )
    ( select-individual p ) ( terpri )
    ( format t "Sampling ... ~%~%" )
    ( select-individual p ) ( terpri )
)

( defmethod mutate ( ( i individual ) &aux mutation )
    ( setf mutation ( mutation ( individual-melody i ) ) )
    ( make-instance 'individual
        :number ( individual-number i )
        :melody mutation
        :fitness ( funcall *fitness* mutation )
    )
)

( defconstant *pc-m* 50 )

( defmethod maybe-mutate ( ( i individual ) )
    ( if ( <= ( + 1 ( random 100 ) ) *pc-m* )
        ( mutate i )
        i
    )
)

( defmethod mutate-demo ()
    ( setf i ( random-individual ) )
    ( display i )
    ( dotimes ( x 20 )
        ( setf i ( mutate i ) )
        ( display i )
    )
)

( defmethod maybe-mutate-demo ()
    ( setf i ( random-individual ) )
    ( display i )
    ( dotimes ( x 20 )
        ( setf n ( maybe-mutate i ) )
        ( display-nnl n )
        ( if ( not ( equal n i ) ) ( princ " *" ) )
        ( terpri )
        ( setf i n )
    )
)

( setf *copy-demo* nil )

( defconstant *pc-c* 40 )

( defmethod perform-copies ( ( cp population ) ( np population ) )
    ( dotimes ( i ( nr-copies ) )
        ( perform-one-copy cp np )
    )
)

( defmethod nr-copies ()
    ( * ( / *pc-c* 100 ) *population-size* )
)

( defmethod perform-one-copy ( ( cp population ) ( np population ) 
    &aux x m mm new-i )
    ( setf m ( select-individual cp ) )
    ( if *copy-demo* ( format t "Selected individual = ~%" ) )
    ( if *copy-demo* ( display m ) )
    ( setf mm ( maybe-mutate m ) )
    ( if *copy-demo* ( format t "Possibly mutated individual = ~&" ) )
    ( if *copy-demo* ( display mm ) )
    ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
    ( if *copy-demo* ( format t "Renumbered individual = ~& " ) )
    ( if *copy-demo* ( display mm ) )
    ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-melody mm ) ) )
    ( setf
        ( population-individuals np )
        ( append ( population-individuals np ) ( list new-i ) )
    )
    nil   
)

( defmethod empty-population ( ( cp population ) &aux np ) 
    ( setf np ( make-instance 'population ) )
    ( setf ( population-individuals np ) () )
    ( setf ( population-generation np ) ( + 1 ( population-generation cp ) ) )
    np
)

( defmethod perform-copies-demo ( &aux cp np )
    ( setf cp ( initial-population ) )
    ( setf np ( empty-population cp ) )
    ( format t "-------------------------------------------------------------------" )
    ( display np )
    ( format t "~%~%---------------------------------------------------------------" )
    ( setf *select-demo* t )
    ( setf *copy-demo* t )
    ( dotimes ( i 10 )
        ( perform-one-copy cp np )
        ( format t "-------------------------------------------------------------------" )
        ( display np )
        ( format t "~%~%-------------------------------------------------------------------" )
    )
    ( setf *select-demo* nil )
    ( setf *copy-demo* nil )
    nil
)

( setf *crossover-demo* nil )

( defconstant *pc-x* 60 )

( defmethod perform-crossovers ( ( cp population ) ( np population ) )
    ( dotimes ( i ( nr-crossovers ) )
        ( perform-one-crossover cp np )
    )
)

( defmethod nr-crossovers ()
    ( * ( / *pc-x* 100 ) *population-size* )
)

( defmethod perform-one-crossover ( ( cp population ) ( np population ) )
    ( let ( x m mm mother father new-i )
        ( setf mother ( select-individual cp ) )
        ( setf father ( select-individual cp ) )
        ( if *crossover-demo* ( format t "Selected mother = ~%" ) )
        ( if *crossover-demo* ( display mother ) )
        ( if *crossover-demo* ( format t "Selected father = ~&" ) )
        ( if *crossover-demo* ( display father ) )
        ( setf m ( crossover mother father ) )
        ( if *crossover-demo* ( format t "the crossover = ~&" ) )
        ( if *crossover-demo* ( display m ) )
        ( setf mm ( maybe-mutate m ) )
        ( if *crossover-demo* ( format t "the possibly mutated individual = ~&" ) )
        ( if *crossover-demo* ( display mm ) )
        ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
        ( if *crossover-demo* ( format t "the renumbered individual = ~&" ) )
        ( if *crossover-demo* ( display mm ) )
        ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-melody mm ) ) )
        ( setf 
            ( population-individuals np )
            ( append ( population-individuals np ) ( list new-i ) )
        )
    )
    nil
)

( defmethod crossover ( ( mother individual ) ( father individual ) 
    &aux mi fi x i )
    ( setf mi ( individual-melody mother ) )
    ( setf fi ( individual-melody father ) )
    ( setf x ( crossover mi fi ) )
    ( setf i ( new-individual 0 x ) )
    i
)

( defmethod perform-crossovers-demo ( &aux cp np )
    ( setf cp ( initial-population ) )
    ( setf np ( empty-population cp ) )
    ( format t "-------------------------------------------------------------------")
    ( setf *select-demo* t )
    ( setf *crossover-demo* t )
    ( dotimes ( i 10 )
        ( perform-one-crossover cp np )
        ( format t "-------------------------------------------------------------------" )
        ( display np )
        ( format t "~%~%---------------------------------------------------------------" )
    )
    ( setf *select-demo* nil )
    ( setf *crossover-demo*  nil )
    nil
)

;; THE NEXT GENERATION METHOD FOR THE GA

( defmethod next-generation ( ( cp population ) &aux np )
    ( setf np ( empty-population cp ) )
    ( perform-copies cp np )
    ( perform-crossovers cp np )
    np
)

;; THE GA!
( defconstant *nr-generations* 25 )

( defmethod ga ( &aux p )
    ( format t "STEPWISE MOTION MELODIES! ~%~%" )
    ( setf *fitness* #'fitness-stepwise-motion )
    ( setf p ( initial-population ) )
    ( terpri )
    ( summarize p )
    ( dotimes ( i *nr-generations* )
        ( setf p ( next-generation p ) )
        ( check-average p )
    )
    ( terpri )
    ( summarize p )
    ( format t "PAIR-WISE MELODIES! ~%~%" )
    ( setf *fitness* #'fitness-pairs )
    ( dotimes ( i *nr-generations* )
        ( setf p ( next-generation p ) )
        ( check-average p )
    )
    ( terpri )
    ( summarize p )
    ( format t "ASCENDING JUMPS AND STEPWISE DECLINES MELODIES! ~%~%" )
    ( setf *fitness* #'fitness-ascending-jumps )
    ( dotimes ( i *nr-generations* )
        ( setf p ( next-generation p ) )
        ( check-average p )
    )
    ( terpri )
    ( summarize p )
    ( format t "STEPWISE SLASHING MELODIES! ~%~%" )
    ( setf *fitness* #'fitness-stepwise-slashing )
    ( dotimes ( i *nr-generations* )
        ( setf p ( next-generation p ) )
        ( check-average p )
    )
    ( terpri )
    ( summarize p )
    ( format t "ZIG-ZAG MELODIES! ~%~%" )
    ( setf *fitness* #'fitness-zig-zag )
    ( dotimes ( i *nr-generations* )
        ( setf p ( next-generation p ) )
        ( check-average p )
    )
    ( terpri )
    ( summarize p )
)

;; METHODS TO PROVIDE INFORMATION ON "PROGRESS"

( defmethod summarize ( ( p population ) )
    ( display p )
    ( check-average p )
    ( terpri )
)

( defmethod check-average ( ( p population ) )
    ( format t "avearge fitness of population ~A = ~A~%" 
        ( population-generation p )
        ( average p )
    )
)