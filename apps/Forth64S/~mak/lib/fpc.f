\ TTTTTTTTTTTTTTTTTTTTT
\ REQUIRE $+!

S" ~mak/place.f" INCLUDED


REQUIRE ?SLITERAL5_H ~mak/lib/fpcnum.f 
\ REQUIRE CODE	lib\ext\spf-asm.f

 ' ?SLITERAL4_H TO ?SLITERAL

\- /STRING : /STRING DUP >R - SWAP R> + SWAP ;

\- INCR : INCR  1+! ;

\- BOUNDS : BOUNDS OVER + SWAP ;


[IFNDEF]  .R
: .R    ( n1 n2 -- )    \ display n1 as a hex number right
                        \ justified in a field of n2 characters
          >R
          0 <# #S #> R> OVER - 0 MAX SPACES TYPE
          ;
[THEN]

[UNDEFINED] R>DROP
[IF]   : R>DROP POSTPONE RDROP ; IMMEDIATE
[THEN]


: (EXEC:) CELLS R> + @ >R ;

: EXEC:  ( -- )
  S" ;" EVALUATE  -1 ALLOT
  POSTPONE (EXEC:)
  BEGIN
    PARSE-NAME DUP 0=
    IF  NIP  REFILL   0= IF DROP TRUE THEN
    ELSE 2DUP S" ;" COMPARE
         IF  SFIND 0=  IF -321 THROW THEN (  -? )
             , FALSE
         ELSE 2DROP TRUE
         THEN
    THEN
  UNTIL
; IMMEDIATE

0 [IF]
CODE FLIP       ( n1 -- n2 )  \ Exchange the high and low halves of a word
     XCHG AL, AH
     RET
END-CODE
[THEN]

[IFNDEF] UPPER
 : UPPER ( A L -- )
\        SWAP CharUpperBuff drop
         OVER + SWAP
         ?DO I C@ DUP [CHAR] Z U>
            IF  0xDF AND
            THEN  I C!
         LOOP ;
[THEN]

[IFNDEF] ?UPPERCASE
VARIABLE CAPS           \ Flag: if true, convert names to upper case.
 TRUE CAPS !
: ?UPPERCASE ( a1 -- a1 )
\ Conditionally convert a counted string to upper case
  CAPS @ 
  IF  DUP COUNT UPPER
  THEN
;

\- 2,  : 2,  ( D -- ) HERE 2! 2 CELLS ALLOT ;

\- 0> : 0> 1- 0< 0= ;

\- 2CONSTANT : 2CONSTANT  CREATE 2, DOES> 2@ ;

: ASCII    CHAR          STATE @ IF LIT, THEN ; IMMEDIATE

: SPLIT DUP  0xFF AND SWAP 8 RSHIFT ;

: JOIN 8 LSHIFT OR ;

[IFNDEF] ?LEAVE
: ?LEAVE
 POSTPONE IF
 POSTPONE LEAVE
 POSTPONE THEN
;  IMMEDIATE
[THEN]

[IFNDEF] WL_NEAR_NFA_N
: WL_NEAR_NFA_N ( addr nfa - addr nfa | addr 0 )
   BEGIN 2DUP DUP IF NAME> THEN U<
   WHILE CDR
   REPEAT
;

: N_UMAX ( nfa nfa1 -- nfa|nfa1 )
 OVER DUP IF NAME> THEN
 OVER DUP IF NAME> THEN U< IF NIP EXIT THEN DROP ;

: WL_NEAR_NFA_M (  addr wid - nfa2 addr | 0 addr )
   0 -ROT
   CELL+ @
   BEGIN  DUP
   WHILE  WL_NEAR_NFA_N  \  nfa addr nfa1
       SWAP >R 
       DUP  >R  N_UMAX 
       R>  DUP  IF CDR THEN
       R>  SWAP
   REPEAT DROP
;

: NEAR_NFA ( addr - nfa addr | 0 addr )
   0 SWAP 
   VOC-LIST
   BEGIN  @ DUP
   WHILE  DUP  >R   WL_NEAR_NFA_M
   >R  N_UMAX  R>  R>
   REPEAT DROP
;

[THEN]

: >NAME    ( CFA -- NFA  )
 NEAR_NFA DROP ;


\ : OK EDIT_FN COUNT
