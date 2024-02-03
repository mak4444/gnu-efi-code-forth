
REQUIRE $!  ForthLib\tools\place.4th

\- DALLOT : DALLOT ALLOT ;
\- TO_SOURCE : TO_SOURCE DUP #TIB ! TIB SWAP MOVE ;

VARIABLE XSOURCE
 0x101 DALLOT
0 VALUE X>IN
0 VALUE XDP
VARIABLE XFP CELL DALLOT
VARIABLE XCURSTR
\ .( DP=) dp h. cr
: [BEGIN]
 >IN @ TO X>IN  
  HERE TO XDP  
  SOURCE XSOURCE $! 
  SOURCE-ID FILE-POSITION
 DROP
\ CR ." XFP=<"  2DUP H. H. ." ><"  SOURCE type   ." >"
 XFP 2!
  CURSTR @ XCURSTR !
; IMMEDIATE


: [AGAIN]
   XSOURCE COUNT TO_SOURCE
   XCURSTR @ CURSTR !
   X>IN  >IN  !
   XDP   DP   !
   XFP 2@
\ CR ." AGXFP=<"  2DUP H. H. ." ><"  SOURCE type   ." >"
  SOURCE-ID
 REPOSITION-FILE DROP
; IMMEDIATE

: [UNTIL]
  IF POSTPONE [AGAIN] THEN
; IMMEDIATE


\EOF test

0 VALUE XN

: 2: :
  1 TO XN
 POSTPONE [BEGIN]
 ;

: 2;
 XN IF
   XN 1- TO XN
   POSTPONE [AGAIN]
   EXIT
 THEN
 POSTPONE ; ; IMMEDIATE

2: HI   ." Hello!!"
 [ CR .( PASS=) XN . ]
2;

: HA   ." HoHo!!"
 [ CR .( PASS=) XN . ]
;

2: HU   ." HUUU!!"
 [ CR .( PASS=) XN . ]
2;

HI HA
