

: HOLD ( char -- ) \ 94
  HLD @ 1- DUP HLD ! C!
;

: <# ( -- ) \ 94
  PAD 1- HLD !
  0 PAD 1- C!
;

: # ( ud1 -- ud2 ) \ 94
  0 BASE @ UM/MOD >R BASE @ UM/MOD R>
  ROT DUP 10 < 0= IF 7 + THEN
 48 + 
  HOLD
;

: #S ( ud1 -- ud2 ) \ 94
  BEGIN
    # 2DUP D0=
  UNTIL
;

: #> ( xd -- c-addr u ) \ 94
  2DROP HLD @ PAD OVER - 1-
;

: SIGN ( n -- ) \ 94
  0< IF [CHAR] - HOLD THEN
;

: (D.) ( d -- addr len )
  DUP >R DABS <# #S R> SIGN #>
;

: HOLDS ( addr u -- ) \ from eserv src
  TUCK + SWAP 0 ?DO
 DUP I
 - 1- C@ HOLD ( /CHAR +LOOP FIXME) LOOP DROP

;

: SPACES       ( N  -- )
    0MAX 80 MIN ?DUP
    IF      0 ?DO SPACE LOOP 
    THEN
;

: D. ( d -- ) \ 94 DOUBLE
  (D.) TYPE SPACE ;

: S>U  0 ;

: . ( n -- )  S>D D. ;

: U. ( u -- )   0 D. ;

\ : U.R ( u n -- ) \ 94 CORE EXT
\  >R  0 <# #S #>  R> OVER - 0MAX SPACES TYPE ;

: U.R  ( u n -- ) \ 94 CORE EXT
  >R 0 (D.) R> OVER - SPACES TYPE  ;

T: .R  ( n1 n -- ) \ 94 CORE EXT
  >R S>D (D.) R> OVER - SPACES TYPE  ;

: H.R ( n1 n2 -- )
 BASE @  >R HEX U.R
 R> BASE ! ;

: H.N           ( n1 n2 -- )    \ display n1 as a HEX number of n2 digits
                BASE @ >R HEX >R
                0 <# R> 0 ?DO # LOOP #> TYPE
                R> BASE ! ;


: H.H C@ 2 H.N SPACE ;
\ : TAB 9 EMIT ;

: .SN ( n --)
  DUP 0< IF ." Stack exhaustion" SP0 @ SP! THEN
   >R BEGIN
         R@
      WHILE
        SP@ R@ 1- CELLS + @ .
        R> 1- >R
      REPEAT RDROP
;

T: .S DEPTH .SN ;

: H.
  BASE @ HEX SWAP U. BASE ! ;


: 0.R
  >R 0 <# #S #> R> OVER - 0 MAX DUP 
    IF 0 DO [CHAR] 0 EMIT LOOP
    ELSE DROP THEN TYPE ;


: >PRT
  DUP BL U< IF DROP [CHAR] . THEN
;

: PTYPE
  0 ?DO
 COUNT >PRT EMIT LOOP DROP ;

CREATE DUMP_BASE 0 ,

: DUMP_X ( addr u -- ) \ 94 TOOLS
  DUP 0= IF 2DROP EXIT THEN
  BASE @ >R HEX
  $F + $10 U/ 0 DO
\    I IF CR THEN
    CR DUP DUMP_BASE @ + 4 0.R SPACE
    SPACE DUP $10 0
      DO I 4 MOD 0= IF SPACE THEN
        DUP C@ 2 0.R SPACE 1+
      LOOP SWAP $10  PTYPE
  LOOP DROP R> BASE !
;

T: DUMP ( addr u -- ) \ 94 TOOLS
  DUMP_BASE 0!
  CR DUMP_X
;

: DIGIT ( C, N1 -> N2, TF / FF ) 
\ N2 - 
\ 
   >R
   [CHAR] 0 - 10 OVER U<
   IF 
      DUP [CHAR] A [CHAR] 0 -     < IF  RDROP DROP 0 EXIT      THEN
      DUP [CHAR] a [CHAR] 0 -  1- > IF [CHAR] a  [CHAR] A - -  THEN
          [CHAR] A [CHAR] 0 - 10 - -
   THEN R> OVER U> DUP 0= IF NIP THEN ;


: >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 ) \ 94
  BEGIN
    DUP
  WHILE
    >R
    DUP >R
    C@ BASE @ DIGIT 0=     \ ud n flag
    IF R> R> EXIT THEN     \ ud n  ( ud = udh udl )
    SWAP BASE @ UM* DROP   \ udl n udh*base
    ROT BASE @ UM* D+      \ (n udh*base)+(udl*baseD)
    R> 1+ R> 1-
  REPEAT
;


\  0 0 S" 1234"  >NUMBER H. H. H. H.

: /STRING DUP>R - SWAP R> + SWAP ;

VARIABLE &DOUBLE?
VARIABLE &?MINUS

: DOUBLE? &DOUBLE? @ ;
: ?MINUS  &?MINUS @ ;

: NUMBER?       ( addr len -- d1 f1 )
                FALSE &DOUBLE? !                \ initially not a double #
                OVER C@ [CHAR] - =
                OVER AND DUP>R
                IF      1 /STRING
                THEN
                DUP 0=
                IF      R>DROP      FALSE &?MINUS !
                        2DROP 0 0 FALSE EXIT   \ always return zero on failure
                THEN
                0 0 2SWAP >NUMBER
                OVER C@ [CHAR] . =              \ next char is a '.'
                OVER SWAP 0< AND                     \ more chars to look at
                IF \     DUP 1- TO DP-LOCATION
			BEGIN
                        1 /STRING >NUMBER
                        DUP 0=
                        IF      TRUE &DOUBLE? ! \ mark as a double number
                        THEN
  OVER C@ [CHAR] . <>			UNTIL 
                THEN    NIP 0=
                R> ?MINUS XOR
                IF      >R DNEGATE R>
                THEN  FALSE &?MINUS !
;

