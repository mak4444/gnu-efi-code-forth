REQUIRE THERE? ~mak/lib/THERE/there.f 

WARNING @ WARNING 0!

: READ-FILE 2>R DUP THERE? IF 2R> TREAD-FILE BREAK 2R> READ-FILE ;

: DP! DP M! ;

: , ( x -- ) \ 94
\ 
  DP M@
 8   ALLOT
 !
 ;

: I, , ;


: L, ( x -- ) \ 94
\ 
  DP M@
 4  ALLOT
 L!
 ;

: LL, ( x -- ) \ 94
\ 
  DP M@
 4  ALLOT
 L!
 ;

: SL@ L@ $80000000	XOR $80000000 - ;

: C, ( char -- ) \ 94
\ 
  DP M@
 1 ALLOT
 C!
 ;


: W, ( word -- )
\ 
  DP M@ 2 ALLOT
 W!
 ;

: +! ( N ADDR -- )	DUP @ ROT + SWAP ! ;

: 1+! ( ADDR -- )	1 SWAP +! ;

\- MCOUNT : MCOUNT COUNT ;
: COUNT DUP 1+ SWAP C@ ;
: WCOUNT DUP 2+ SWAP W@ ;

\- >PRT : >PRT  DUP BL U< IF DROP [CHAR] . THEN ;

: PTYPE
  0 DO COUNT >PRT EMIT LOOP DROP
;


\+ DUMP : MDUMP ( addr u -- ) DUMP ;

: DUMP ( addr u -- ) \ 94 TOOLS
  DUP 0= IF 2DROP EXIT THEN
  BASE M@ >R HEX
  15 + 16 U/ 0 DO
    CR DUP 4 .0 SPACE
    SPACE DUP 16 0
      DO I 4 MOD 0= IF SPACE THEN
        DUP
        C@
        2 .0 SPACE 1+
      LOOP SWAP 16  PTYPE
  LOOP DROP R> BASE M!
;


VECT TTT_C@
: TTT_C@_SET  TO TTT_C@ ;

' ABORT TTT_C@_SET

: DDPPP ( addr u -- ) \ 94 TOOLS
  DUP 0= IF 2DROP EXIT THEN
  BASE M@ >R HEX
  7 + 8 U/ 0 DO
    CR DUP 4 .0 SPACE
    SPACE DUP 8 0
      DO I 4 MOD 0= IF SPACE THEN
        DUP  ." |"
        TTT_C@
        ." <" 2 .0 ." >" 1+
      LOOP SWAP  DROP \  8  PTYPE
  LOOP DROP R> BASE M!
;

: COUNT DUP 1+ SWAP C@ ;

: T_PTYPE
  0 DO DUP 1+ SWAP T_C@ >PRT EMIT LOOP DROP
;

: TDUMP ( addr u -- ) \ 94 TOOLS
  DUP 0= IF 2DROP EXIT THEN
  BASE M@ >R HEX
  15 + 16 U/ 0 DO
    CR DUP 4 .0 SPACE
    SPACE DUP 16 0
      DO I 4 MOD 0= IF SPACE THEN
        DUP
        T_C@
        2 .0 SPACE 1+
      LOOP SWAP 16  T_PTYPE
  LOOP DROP R> BASE M!
;

: C!A TUCK C! ;

: FILL ( c-addr u c ---)
\ Fill a block of u bytes starting at c-addr with character c.
  OVER IF >R 
  BEGIN
   R@ ROT C!A 1+ SWAP   
   1- DUP 0= 
  UNTIL
  R>
  THEN
  DROP 2DROP
;     

WARNING !

