
REQUIRE [IF] ~mak/CompIF4.f
REQUIRE [IFNDEF] ~nn/lib/ifdef.f
\ REQUIRE _INF_MOD ~mak/infix3.f 

[IFNDEF] /STRING : /STRING DUP >R - SWAP R> + SWAP ;
[THEN]

[IFNDEF] 0>  : 0> 0 > ;
[THEN]

[IFNDEF] R>DROP : R>DROP POSTPONE RDROP ; IMMEDIATE
[THEN]

[IFNDEF] DUP>R : DUP>R POSTPONE DUP POSTPONE >R ; IMMEDIATE
[THEN]

BASE @

HEX
[IFNDEF] UPC
: UPC  ( c -- c' )
   DUP [CHAR] Z U>
   IF  DF AND
   THEN   ;
[THEN]

0 VALUE DOUBLE?

-1 VALUE DP-LOCATION

0 VALUE ?MINUS

: NUMBER?       ( addr len -- d1 f1 )
                FALSE TO DOUBLE?                \ initially not a double #
                -1 TO DP-LOCATION
                OVER C@ [CHAR] - =
                OVER SWAP 0< AND DUP>R
                IF      1 /STRING
                THEN
                DUP 0=
                IF      R>DROP      FALSE TO ?MINUS
                        2DROP 0 0 FALSE EXIT   \ always return zero on failure
                THEN
                0 0 2SWAP >NUMBER
                OVER C@ [CHAR] . =              \ next char is a '.'
                OVER SWAP 0< AND                     \ more chars to look at
                IF      DUP 1- TO DP-LOCATION
			BEGIN
                        1 /STRING >NUMBER
                        DUP 0=
                        IF      TRUE TO DOUBLE? \ mark as a double number
                        THEN
  OVER C@ [CHAR] . <>			UNTIL 
                THEN    NIP 0=
                R> ?MINUS XOR
                IF      >R DNEGATE R>
                THEN  FALSE TO ?MINUS
;

: SNUMBER ( addr len -- d1 )
 NUMBER? THROW ;
 

: NUMBER ( a1 -- d1 )
\ Convert count delimited string at a1 into a double number.

\  0 0 ROT COUNT >NUMBER THROW DROP

 COUNT
 NUMBER? 0=
 THROW ;

: NUMBER,      ( d -- )
                DOUBLE? 0= IF DROP THEN
                STATE @
                IF      DOUBLE? IF  SWAP  LIT,  THEN
                        LIT,
                THEN
;

: XXX-SLITERAL ( addr u -> d true | false ) 
   NUMBER?
 IF NUMBER, TRUE  EXIT
 THEN
   2DROP FALSE
;

: BIN-SLITERAL ( addr u -> d true | false )
  BASE @ >R 2 BASE !
  XXX-SLITERAL
  R> BASE !
;

: HHH-SLITERAL ( addr u -> d true | false )
  BASE @ >R HEX
  2- SWAP 2+ SWAP
  XXX-SLITERAL
  R> BASE !
;

: DEC-SLITERAL ( addr u -> d true | false )
  BASE @ >R DECIMAL
  XXX-SLITERAL
  R> BASE !
;

: ?SLITERAL3_H_  ( c-addr u -- ... )
  DUP 1 >
     IF
	 2DUP 2>R
         OVER C@ [CHAR] - = DUP  TO ?MINUS
         IF    1 /STRING 
         THEN

         OVER W@ 7830 ( 0x) = 
         IF     HHH-SLITERAL
		IF RDROP RDROP
		ELSE -2001 THROW
		THEN EXIT
         THEN
[ DECIMAL ]
          OVER C@ [CHAR] $ = 
         IF 1+ SWAP 1- SWAP HHH-SLITERAL
		IF RDROP RDROP
		ELSE -2001 THROW
		THEN EXIT
         THEN

          OVER C@ [CHAR] # = 
         IF 1- SWAP 1+ SWAP DEC-SLITERAL
		IF RDROP RDROP
		ELSE -2001 THROW
		THEN EXIT
         THEN

          OVER C@ [CHAR] % = 
         IF 1- SWAP 1+ SWAP BIN-SLITERAL
		IF RDROP RDROP
		ELSE -2001 THROW
		THEN EXIT
         THEN

              2DUP + 1- C@ UPC [CHAR] H =
         IF  1+  SWAP 2- SWAP  HHH-SLITERAL
		IF RDROP RDROP
		ELSE -2001 THROW
		THEN EXIT
         THEN
[ HEX ]

             2DUP + 1- C@ UPC [CHAR] B = BASE @ 10 <> AND
[ DECIMAL ]
         IF   1- BIN-SLITERAL
		IF RDROP RDROP 
		ELSE -2001 THROW
		THEN EXIT
         THEN
[ HEX ]

             OVER @ FF00FF  AND 270027 ( '\0')  = 
             OVER 3 = AND
         IF  DROP @ 8 RSHIFT FF AND
            STATE @ IF LIT, THEN RDROP RDROP  EXIT
         THEN 
         2DROP 2R>
  THEN
  2DUP
 2>R 
 XXX-SLITERAL 0= \ ['] ?SLITERAL1 CATCH
  IF
       2R>
       OVER C@ [CHAR] " = OVER 2 > AND
       IF 2 - SWAP 1+ SWAP THEN
       2DUP + 0 SWAP C!
\       2dup type
       ['] INCLUDED CATCH
       DUP 2 = OVER 3 = OR OVER 161 = OR
       
       IF  -2003 THROW \ ABORT"  -???"
       ELSE DROP -D THROW THEN
  ELSE RDROP RDROP
  THEN
;

HEX
-1 VALUE ?HBTEM
: YYY-SLITERAL ( addr u -> d true | false )
  DUP 1 >
     IF
	 2DUP 2>R
         OVER C@ [CHAR] - = DUP  TO ?MINUS
         IF    1 /STRING 
         THEN

         OVER W@ 7830 ( 0x) = 
         IF     HHH-SLITERAL RDROP RDROP  EXIT
         THEN

          OVER C@ [CHAR] $ = 
         IF   1+ SWAP 1- SWAP HHH-SLITERAL
		RDROP RDROP EXIT
         THEN


          OVER C@ [CHAR] # = 
         IF 1- SWAP 1+ SWAP DEC-SLITERAL
		 RDROP RDROP EXIT
         THEN

          OVER C@ [CHAR] % = 
         IF 1- SWAP 1+ SWAP BIN-SLITERAL
		 RDROP RDROP EXIT
         THEN

	?HBTEM
	IF
              2DUP + 1- C@ 20 OR [CHAR] h =
         IF  1+  SWAP 2- SWAP  HHH-SLITERAL
		RDROP RDROP EXIT
         THEN

             2DUP + 1- C@ 20 OR [CHAR] b = BASE @ 10 <> AND
         IF   1- BIN-SLITERAL
		RDROP RDROP EXIT
         THEN
	THEN

             OVER @ FF00FF  AND 270027 ( '\0')  = 
             OVER 3 = AND
         IF  DROP @ 8 RSHIFT FF AND
		STATE @ IF LIT, THEN
             RDROP RDROP TRUE EXIT
         THEN 
         2DROP 2R>
  THEN
 XXX-SLITERAL
;
DECIMAL
: ?SLITERAL3_H  ( c-addr u -- ... )
  2DUP 2>R 
    YYY-SLITERAL 0=
    IF  2R>	
       OVER C@ [CHAR] " = OVER 2 > AND
       IF 2 - SWAP 1+ SWAP THEN
       2DUP + 0 SWAP C!
\       ." IN=<" 2DUP TYPE ." >" CR
       ['] INCLUDED CATCH
       DUP 2 = OVER 3 = OR OVER 161 = OR
       IF  -2003 THROW \ ABORT"  -???"
       ELSE -13  THROW THEN EXIT
   THEN
   RDROP RDROP
;

\ ' XXX-SLITERAL  TO ?SLITERAL
 ' ?SLITERAL3_H_ TO ?SLITERAL

BASE !
