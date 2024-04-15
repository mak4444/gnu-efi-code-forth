REQUIRE [IF] ForthLib\tools\CompIF3.4th
REQUIRE [IFNDEF]  ForthLib\tools\ifdef.4th
REQUIRE $!  ForthLib\tools\place.4th
REQUIRE CASE ForthLib\ext\case.4th
REQUIRE NUMBER? ForthLib\lib\fpcnum.4th
REQUIRE NEAR_NFA ForthLib\spf\near_nfa.4th 
REQUIRE MODULE: ForthLib\ext\spf_modules.4th 

\- U>D 0 CONSTANT U>D
\- H. : H.  BASE M@ HEX SWAP U. BASE !  ;
\- OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ;
\- THERE : THERE HERE ;
[IFNDEF] BOUNDS : BOUNDS OVER + SWAP ;
[THEN]

[IFNDEF] W>S
: W>S ( w -- n )  \ 
  0xFFFF AND  0x8000  XOR 0x8000 - ;
[THEN]

[IFNDEF] L>S
: L>S ( w -- n )  \ 
  0xFFFFFFFF AND  0x80000000  XOR 0x80000000 - ;
[THEN]

[IFNDEF] BLANK
: BLANK         ( addr len -- )     \ fill addr for len with spaces (blanks)
                BL FILL ;
[THEN]

[IFNDEF] (D.)
: (D.)          ( d -- addr len )       TUCK DABS  <# #S ROT SIGN #> ;
[THEN]

[IFNDEF] H.R
: H.R    ( n1 n2 -- )    \ display n1 as a hex number right
                        \ justified in a field of n2 characters
          BASE @ >R HEX >R
          0 <# #S #> R> OVER - 0 MAX SPACES TYPE
          R> BASE ! ;
[THEN]

\- #TAB 9 CONSTANT  #TAB
\- TAB : TAB #TAB EMIT ;

[IFNDEF] H.N
: H.N           ( n1 n2 -- )    \ display n1 as a HEX number of n2 digits
                BASE @ >R HEX >R
                0 <# R> 0 ?DO # LOOP #> TYPE
                R> BASE ! ;
[THEN]

\- BREAK	: BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE
\- <<	: << LSHIFT ;

\- >>	: >> RSHIFT ;
\- A>>	: A>> ARSHIFT ;

\- ANDC : ANDC INVERT AND ;
\- MCOUNT : MCOUNT COUNT ;
\- ATYPE : ATYPE TYPE ;

\- .- : .-  S>D (D.) TYPE ;
\- U.- : U.-  0 (D.) TYPE ;
\- H.- : H.-   BASE @ HEX SWAP U>D (D.) TYPE BASE ! ;

0 VALUE ADDR_OFF

: T_?.NAME>S      ( CFA -- )
\ ELIMINATE " 0x"
	NEAR_NFA
	>R DUP
	IF ."  ( " DUP MCOUNT TYPE 
	     NAME>  R> - DUP
	     IF   DUP ." +" NEGATE H. \ >S
	     THEN DROP        ."  ) "
	ELSE RDROP DROP
	THEN ;

: F_?.NAME>S      ( CFA -- )
 DUP ." 0x" H. T_?.NAME>S
 ;

: RR.
\ + INLINE?  ." <!"  R@  T_?.NAME>S ( R>  R@ T_?.NAME>S >R)  ." >"
 ;

10 [IF]

\ : ?.NAME>S      ( CFA -- ) G_?.NAME>S ;

' T_?.NAME>S  ->DEFER ?.NAME>S

: .PERFORM 
\ + INLINE?  R@  ?.NAME>S
\ + INLINE?  DUP @ >NAME ." {" COUNT TYPE ." }"
  PERFORM ;
[ELSE]

\  ?.NAME>S      ( CFA -- ) T_?.NAME>S ;
' T_?.NAME>S  ->DEFER ?.NAME>S

: .PERFORM 
  r@  ?.NAME>S
  dup @ >name ." {" count type  ." }"
  PERFORM ;
[THEN]
\+ CASE-INS CASE-INS ON
\- HEX. : HEX. H. ;
\- \G : \G POSTPONE \ ; IMMEDIATE
[THEN]

[IFNDEF] BASE-EXECUTE
: BASE-EXECUTE ( i*x xt u -- j*x ) \ gforth
    \G execute @i{xt} with the content of @code{BASE} being @i{u}, and
    \G restoring the original @code{BASE} afterwards.
  BASE @ >R BASE ! EXECUTE R> BASE ! ;
[THEN]      

[IFNDEF] -TRAILING
: -TRAILING  ( c_addr u1 -- c_addr u2 ) \ string dash-trailing
\G Adjust the string specified by @i{c-addr, u1} to remove all
\G trailing spaces. @i{u2} is the length of the modified string.
    BEGIN
	DUP
    WHILE
	1- 2DUP + C@ BL <>
    UNTIL  1+  THEN ;
[THEN]      



\- endif : endif POSTPONE THEN ; IMMEDIATE
\- 2, : 2, , , ;
\- DEFER : DEFER VECT ;
\+ ALIGN-BYTES 8 ALIGN-BYTES !

: D# BASE @ >R DECIMAL PARSE-NAME EVALUATE R> BASE ! ;  IMMEDIATE
\- BOUNDS : BOUNDS OVER + SWAP ;
\- ," : ," '"' PARSE S", ;
\- <= : <= > 0= ;

\- SELECT : SELECT IF DROP BREAK NIP ;
\- SW@ : SW@ W@ $8000		XOR $8000 - ;
\- SL@ : SL@ L@ $80000000	XOR $80000000 - ;
\- IS  : IS POSTPONE TO ; IMMEDIATE


: ?x.NAME>S      ( CFA -- )
 ?.NAME>S ;

0 VALUE //WAS
: //.DO  ." /*"  TRUE TO //WAS ;

' NOOP ->DEFER //.

DEFER .CODE

MODULE: DISARM
\+ INLINE?

 BASE @ HEX

VARIABLE CP?
0 VALUE STRBYTE?
0 VALUE DOVECT?
0 VALUE XDO?
HERE 0 C, VALUE CODNAME

: CODNAME.- TAB CODNAME COUNT TYPE ;
: CODNAME.  CODNAME.- TAB ;

\ long words and presigns

: .#    '#' EMIT ;
: .$    ." 0x" ;
: .,    ',' EMIT ;
: .+    '+' EMIT ;
: .-    '-' EMIT ;
: ..    '.' EMIT ;
: .:    ':' EMIT ;
: .[    '(' EMIT ;
: .]    ')' EMIT ;
: #.R ( n -- ) \ print decimal
    0 ['] .R D# 10 BASE-EXECUTE ;

\ signed / unsigned byte, word and long output         07aug10py

: .LFORMAT   ( addr -- )  $C U.R ." :" ;

: .$DU  ( N -- )       .$ H.- ;
: .$DS  ( N -- )
\+ 'FSTART  DUP 'FSTART - 0xFFFFFF U<  IF	." fimg+0x" 'FSTART - H.-  BREAK
       DUP 0< IF  .- NEGATE  THEN  .$DU ;

: .HBY   ( 8B -- )     BASE @ >R HEX  0  <#  # #  #>  TYPE R> BASE ! ;
: .$BU  ( 8B -- )      .$ .HBY ;
: .$BS  ( 8B -- )      $FF AND DUP $7F > IF .- $100 SWAP - THEN .$BU  ;

\ Variables and tabs

VARIABLE OPCODE
VARIABLE MODE
VARIABLE LENGTH
VARIABLE ALENGTH
VARIABLE SEG: SEG: ON
VARIABLE REX
VARIABLE MAP-SELECT
VARIABLE VVVV

  D# 36 CONSTANT  BYTFLD
  D# 10 CONSTANT  MNEFLD
  D# 18 CONSTANT  ADDRFLD

: LEN!  0 LENGTH !  0 ALENGTH !  SEG: ON ;

\ Strings                                              07feb93py
CREATE "REGS  ," axcxdxbxspbpsidi8 9 101112131415"
CREATE "BREG  ," al  cl  dl  bl  ah  ch  dh  bh  r8l r9l r10lr11lr12lr13lr14lr15l"
CREATE "BREG2 ," al  cl  dl  bl  spl bpl sil dil r8l r9l r10lr11lr12lr13lr14lr15l"
CREATE "16RI ," bx+sibx+dibp+sibp+disi   di   bp   bx   "
CREATE "PTRS ," dwordword byte "
CREATE "IDX  ," ,1,2,4,8"
CREATE "SEG  ," ESCSSSDSFSGS"   CREATE "SEG1 ," escsssdsfsgs"
CREATE "JMP ," o b z bes p l le"
CREATE GRP1 ," addor adcsbbandsubxorcmprolrorrclrcrshlshrsalsar"
CREATE GRP3 ," testtestnot neg mul imuldiv idiv"
CREATE GRP4 ," inc  dec  call callfjmp  jmpf push g4?? "
CREATE GRP6  ," sldtstr lldtltr verrverwg6??g6??sgdtsidtlgdtlidtsmswg7??lmswg7??"
CREATE GRP8 ," ???? src"
2 "REGS C!      5 "16RI C!      5 "PTRS C!      2 "IDX  C!
2 "SEG  C!      2 "SEG1 C!      2 "JMP  C!      3 GRP1  C!
4 GRP3  C!      5 GRP4  C!      4 GRP6  C!      1 GRP8  C!
4 "BREG C!      4 "BREG2 C!

\ rex handling


: REX? ( N -- FLAG )  REX @ AND 0<> ;
: P? ( -- FLAG ) $40 REX? ;
: W? ( -- FLAG )  $8 REX? ;
: SS? ( -- FLAG ) D# 20 REX? ;
: SD? ( -- FLAG ) D# 10 REX? ;
: S? ( -- FLAG ) D# 30 REX? ;
: L? ( -- FLAG ) D# 80 REX? ;
: VEX? ( -- FLAG ) $100 REX? ;

: >R? ( REG -- REG' )   4 REX? $8 AND OR ;
: >X? ( REG -- REG' )   2 REX? $8 AND OR ;
: >B? ( REG -- REG' )   1 REX? $8 AND OR ;

: SUFFIX.
  REX @
 CASE 
 $48 OF 'q' ENDOF
 $66 OF 'w' ENDOF
 $c6 OF 'b' ENDOF
 DROP EXIT
 ENDCASE EMIT ;


\ Register display

: *."  ( n addr -- )  COUNT >R SWAP R@ * + R> -TRAILING TYPE ;
: .REGSIZE ( reg -- reg ) ." %"
    DUP 7 <=  IF  'r' 'e' W?  SELECT EMIT  ELSE  'r' EMIT  THEN  ;
: .(REG ( n l -- ) RR.
    >R R@ 0=	IF  .REGSIZE "REGS ( " )  ELSE
 ." %" R@ 2 =	IF    "BREG2 "BREG P? SELECT  ELSE
  "REGS ( " ) THEN  THEN
    OVER >R *." R> 7 > IF  CASE R@
	    0 OF  W? 0= IF 'D' EMIT  THEN  ENDOF
	    1 OF  'W' EMIT                 ENDOF
	    2 OF  'B' EMIT                 ENDOF
	ENDCASE  THEN  RDROP ;
: .REG  ( N -- )  LENGTH @ .(REG ;
: .R/REG  ( N -- )    >R? LENGTH @ .(REG ;
: .M/REG  ( N -- )    >X? LENGTH @ .(REG ;
: .EREG ( N -- )  .REGSIZE "REGS *." ;
: .MI/REG  ( N -- )   >X?  ." %r" "REGS *."  ;
\ mmo: .SIB/REG  ( N -- )  >B? .EREG ;
: .SIB/REG  ( N -- ) >B? ." %r" "REGS *." ;
: .S/REG  ( N -- )  >B? LENGTH @  .(REG ;
: .SEG  ( N -- )  "SEG *." ;

: MOD@ ( addr -- addr' r/m reg )
  COUNT DUP $C7 AND SWAP 3 RSHIFT 7 AND ;
: .8B  ( addr -- addr' )  COUNT .$BS ;
: .32B ( addr -- addr' )  DUP SL@  .$DS 4 + ;
: .32U ( addr -- addr' )  DUP SL@  OVER +  .$DU 4 + ;
: .64B ( addr -- addr' )  DUP @   .$DS 8 + ;
: .64U ( addr -- addr' )  DUP @   .$DU 8 + ;

\ Register display

CREATE .DISP ' NOOP ,  ' .8B ,   ' .32B ,

: .SIB  ( addr mod -- addr' ) >R COUNT  DUP 7 AND 5 = R@ 0= AND \ IF ." /* ~ */" THEN  0
  IF    RDROP >R .32B R>  .[
  ELSE  SWAP R> CELLS .DISP + .PERFORM SWAP DUP 7 AND .[ .SIB/REG
  THEN  3 RSHIFT DUP 7 AND 4 = 0=
  IF   .,   DUP 7 AND .MI/REG 3 RSHIFT "IDX *."
  ELSE  DROP  THEN  .] ;

: .32A  ( addr r/m -- addr' ) RR.
 DUP 7 AND >R 6 RSHIFT
  DUP 3 =            IF  DROP R>    .S/REG    EXIT  THEN
  DUP 0= R@ 5 = AND  IF  DROP RDROP  DUP SL@  OVER + 4+
			OPCODE C@ 81 =	IF 4+ THEN
			OPCODE C@ 83 =	IF 1+ THEN
			OPCODE C@ C7 =	IF 4+ THEN
			OPCODE C@ C6 =	IF 1+ THEN
			?.NAME>S ." (%rip)" 4+ BREAK
  R@  4 =            IF       RDROP   .SIB    EXIT  THEN
  CELLS .DISP + .PERFORM
  R> .[ .SIB/REG .]
\ + INLINE? CR ." .32A77 REX=" REX @ H.

 ;
\ Register display

: WCOUNT ( addr -- addr' w ) DUP W@ >R 2 + R> ;
: WXCOUNT ( addr -- addr' w ) DUP SW@ >R 2 + R> ;
: +8B  ( addr -- addr' )  COUNT  .$BS ;
: +16B ( addr -- addr' )  WCOUNT .$DS ;

CREATE .16DISP  ' NOOP , ' +8B , ' +16B ,
: .16R  ( reg -- ) .[ "16RI *." .] ;
: .16A  ( addr r/m -- addr' ) $C7 AND
  DUP 6 =  IF  DROP WCOUNT .[ .$DU .] BREAK
  DUP 7 AND >R 6 RSHIFT  DUP 3 =  IF  DROP R> .M/REG BREAK
  CELLS .16DISP + .PERFORM R> .16R  ;


\ Register display

: .ADDR ( addr r/m -- addr' ) RR.
  SEG: @ 0< 0= IF  SEG: @ .SEG ':' EMIT  THEN
  ALENGTH @ IF  .16A  ELSE  .32A  THEN ;

: .MOD  ( addr -- addr' )  MOD@  .R/REG  ., .ADDR  ;
: .RMOD ( addr -- addr' )  MOD@ >R .ADDR R> ., .R/REG ;

: >IMMADDR ( addr1 -- addr1 addr1+ )
	DUP C@ $38 ANDC
	DUP  4  = IF DROP DUP 2+	BREAK
	DUP  5  = IF DROP DUP 5 +	BREAK
	DUP  44 = IF DROP DUP 2+ 1+	BREAK
	DUP  84 = IF DROP DUP 6 +	BREAK
	DUP  8  < IF DROP DUP 1+	BREAK
	F8 AND
	DUP 40  = IF DROP DUP 2+	BREAK
	DUP 80  = IF DROP DUP 5 +	BREAK
	DUP C0  = IF DROP DUP 1+	BREAK
 	DROP DUP
;

: .IMM  ( addr -- addr' ) ." $"  LENGTH @
    DUP 0=	IF  DROP DUP SL@  .$DS 4 + BREAK
  	1 =	IF  WCOUNT .$DS  BREAK  COUNT .$BS ;

: .IMM64  ( addr -- addr' ) ." $"  LENGTH @
    DUP 0= IF  DROP DUP
	W? IF  @ .$DS CELL+  ELSE  L@  .$DS 4 +  THEN
	BREAK
  1 =    IF  WCOUNT .$DS EXIT  THEN  COUNT .$BS ;

DEFER .0FCD

: .B? ( -- ) OPCODE @ 1 AND 0= IF  2 LENGTH !  THEN ;
: .ARI   .B? //. RR. CODNAME.  OPCODE @ DUP 4 AND
  IF DROP .IMM ., 4 - 0 .R/REG 4 + BREAK
  2 AND  IF  .RMOD  ELSE  .MOD  THEN ;
: .MODT CODNAME. .RMOD ;
: .GR  //.  CODNAME. OPCODE @ 7 AND .S/REG ;
: .REXINC    OPCODE @ REX !  .CODE  REX OFF ;
: .REXDEC    OPCODE @ REX ! //. ." REXDEC " .CODE REX OFF ;

: .IGRV //.  CODNAME.  .IMM64 ., OPCODE @ 7 AND  .S/REG   ;

: .IGRB  2 LENGTH ! .IGRV ;
: .IGR .B? CODNAME. .IMM ., 0 .R/REG ;
: .MODB  .B? CODNAME. .RMOD ;

: .XCHA  .GR ., 0 .M/REG ;
\ .conds modifier                                      29may10py


: .COND ( -- ) OPCODE @
  $F AND  DUP 1 AND  IF  'N' EMIT  THEN  2/ "JMP *." ;
: .JB-  TAB  COUNT DUP $80 AND IF -$80 OR THEN OVER + ?.NAME>S ( .$DU) ;
: .JV-  ALENGTH @ IF  WXCOUNT OVER
  ELSE  DUP SL@ SWAP 4 +  TUCK  THEN  + \ DUP .$DU
\+ '(S")	DUP '(S")	= TO STRBYTE?
0
\+ 'DOVECT	OVER 'DOVECT	= OR
\+ '(DO)	OVER '(DO)	= OR
\+ '(?DO)	OVER '(?DO)	= OR
 TO DOVECT?
\+ 'XDO		DUP 'XDO	= TO XDO?
 ?.NAME>S ;
: .JB  CODNAME.- .JB- ;
: .JV  CODNAME.- TAB .JV- ;
: .JM //.  .JV ;
: .JS  CODNAME.- .COND .JB- ;
: .JL  //. CODNAME.- .COND TAB .JV- ;
: .SET  CODNAME.- .COND TAB MOD@ DROP 2 LENGTH ! .ADDR ;

: ASIZE   ALENGTH @ INVERT ALENGTH ! .CODE ;
: OSIZE   LENGTH @ 1 XOR   LENGTH ! .CODE ;
: .SEG:   OPCODE @ 3 RSHIFT 3 AND SEG: ! .CODE ;
: .SEGX   OPCODE @ 1 AND 4 +   SEG: ! .CODE ;
: .PSEG   CODNAME. OPCODE @ 3 RSHIFT 7 AND .SEG ;

: ?REX.X    REX @ IF  REX C@  H.- ." ,0x" THEN ;
: ?REX.
  REX @ 0= IF BREAK
  ." 	rex"
  REX @ $F AND 0= IF SPACE BREAK
  '.' EMIT
  REX @ $8 AND IF 'W' EMIT THEN
  REX @ $4 AND IF 'R' EMIT THEN
  REX @ $2 AND IF 'X' EMIT THEN
  REX @ $1 AND IF 'B' EMIT THEN
; 


\ .GRP1 .GRP4 .ADJ .ARPL
: .GRP1  //. RR.
  OPCODE @ 3 AND  0= IF  ?REX.  THEN

  TAB DUP C@  3 >> 7 AND GRP1 *." TAB
  >IMMADDR OPCODE @ 3 AND 1 <> IF  2 LENGTH !  THEN
 .IMM SWAP ., MOD@ DROP LENGTH OFF .B? .ADDR DROP
;

: .GRP2 RR. .B? MOD@ $8 + GRP1 TAB *." SUFFIX. TAB  OPCODE @ 2 AND IF ." %cl" ELSE ." $1" THEN ., .ADDR  ;

: .GRP3 //. RR. .B? TAB DUP C@  3 >> 7 AND DUP GRP3 TAB *." SUFFIX. TAB

  2 < IF   ." .GRP3.IMM" >IMMADDR .IMM SWAP ., MOD@ DROP .ADDR DROP BREAK
\  MOD@ >R .ADDR  R>  3 > IF .,  0 .R/REG   THEN
  MOD@ DROP .ADDR

 ;


: .GRP4 //. RR. TAB .B? MOD@ DUP GRP4 *." SUFFIX. TAB
  2 + 7 AND   4 < IF  .ADDR  ELSE $8 REX OR! ." *" .ADDR  THEN ;
: .ADJ    OPCODE @ DUP D# 10 AND
  IF  'a'  ELSE  'd'  THEN  EMIT  'a' EMIT  $8 AND
  IF  's'  ELSE  'a'  THEN  EMIT ;
: .SEG#   .[ DUP ALENGTH @ 2* 4 + + WCOUNT .$DU
  .: SWAP ALENGTH @ IF  WCOUNT .$DU  ELSE  .32U  THEN .] DROP ;
\ .MOVO .MOVX .STR                                     23jan93py
: .MOVO   CODNAME. .B?
  OPCODE @ 2 AND	IF  0 .R/REG .,  THEN
  DUP @ ?.NAME>S 8 +
\  .64U
  OPCODE @ 2 AND  0=	IF  ., 0 .R/REG  THEN ;
\ : .MOVX   TAB MOD@ .R/REG ., 1 LENGTH ! .B? .ADDR ;
: .MOVX //.  CODNAME.- SUFFIX. TAB
 MOD@  >R 1 LENGTH ! .B? .ADDR R> ., LENGTH OFF .R/REG ;
: .MOVDX //.  CODNAME. MOD@ >R 0 LENGTH ! .B? .ADDR R> .,  .R/REG ;

: .MOVI //.  CODNAME.-
	OPCODE @ 1 AND IF SUFFIX. ELSE 'b' EMIT THEN TAB .B?
 >IMMADDR .IMM SWAP ., MOD@ DROP .ADDR DROP ;

: .MOVS  CODNAME. MOD@  OPCODE @ 2 AND
 IF  >R .ADDR ., R> ." %" .SEG 
 ELSE ." %" .SEG ., .ADDR 
 THEN ;

: .FAR  CODNAME.- .SEG# ;
0 VALUE TR_FLG
: .MODIV CODNAME. DUP 1+ .IMM ., SWAP .RMOD DROP ;
: .MODIB CODNAME. DUP 1+ 2 LENGTH ! .IMM ., SWAP 0 LENGTH ! .RMOD DROP ;
: .IV     CODNAME. .IMM ;
: .IB     2 LENGTH ! .IV ;
: .EV     CODNAME. MOD@ DROP .ADDR ;
: .ARPL  0 LENGTH ! ." MOVSX" .MOVDX ;

: .IO#  CODNAME. .B? 0 .R/REG ., COUNT .$BU ;
: .RET  CODNAME.-  OPCODE @ 1 AND 0= IF TAB WCOUNT .$DU THEN ;
: .ENTER  CODNAME. WCOUNT ." $" .$DU ., COUNT ." $" .$BU ;
: .STCL OPCODE @ 1 AND IF ." st" ELSE ." cl" THEN
  S" CID " DROP OPCODE @ 2/ 3 AND + C@ EMIT ;

\ .GRP6 .GRP7                                          07aug10py

: .GRP6 TAB RR. ( 1 LENGTH !) MOD@ OPCODE @ 3 LSHIFT + GRP6 *." TAB .ADDR ;

: .GRP2I  //. RR. .B?  TAB DUP C@  3 >> 7 AND $8 + GRP1 *." TAB
  >IMMADDR  2 LENGTH !
 .IMM SWAP ., MOD@ DROP LENGTH OFF .ADDR DROP ;

: .GRP8   ." 	bt" DUP C@  3 >> 7 AND GRP8 *." TAB
  >IMMADDR  2 LENGTH !
 .IMM SWAP ., MOD@ DROP LENGTH OFF .ADDR DROP ;
: .BT     OPCODE @ 3 RSHIFT 7 AND GRP8 *." TAB .RMOD ;
CREATE  LBSWAP  0 C, 3 C, 3 C, 0 C,
: .MOVRX  CODNAME.  OPCODE @ DUP 3 AND LBSWAP + C@ XOR 7 AND >R
  MOD@ R@ 1 AND  IF  SWAP 7 AND .R/REG .,  THEN
  R@ 2/ C" CDT?" + 1+ C@ SWAP 0 <# # 'R' HOLD ROT HOLD #> TYPE
  R> 1 AND  0= IF  ., 7 AND .M/REG  THEN ;
: .LXS CODNAME.-  OPCODE @ 7 AND "SEG1 *." TAB .RMOD ;
: .SHD //.  TAB .RMOD ., 2 LENGTH ! OPCODE @ 1 AND
  IF  1 .R/REG  ELSE ." .SHD.IMM" .IMM  THEN ;

\ .ESC                                                 22may93py
: FLT,  C, BL PARSE THERE OVER 1+ ALLOT $! ;
CREATE FOP1TABLE HEX
80 FLT, CHS     81 FLT, ABS     84 FLT, TST     85 FLT, XAM
08 FLT, LD1     09 FLT, LDL2T   0A FLT, LDL2E   0B FLT, LDPI
0C FLT, LDLG2   0D FLT, LDLN2   0E FLT, LDZ
90 FLT, 2XM1    D1 FLT, YL2X    92 FLT, PTAN    D3 FLT, PATAN
94 FLT, XTRACT  D5 FLT, PREM1   16 FLT, DECSTP  17 FLT, INCSTP
D8 FLT, PREM    D9 FLT, YL2XP1  9A FLT, SQRT    9B FLT, SINCOS
9C FLT, RNDINT  DD FLT, SCALE   9E FLT, SIN     9F FLT, COS
: .ST   ." ST"  ?DUP IF  ." (" 1 .R ." )"  THEN ;
: .ST?  DUP 40 AND IF 1 .ST ., THEN  80 AND IF 0 .ST THEN ;
: .FOP1 ( IP opcode -- IP )  1F AND >R FOP1TABLE
  BEGIN  COUNT 1F AND R@ <  WHILE  COUNT +  REPEAT
  DUP 1- C@ DUP 1F AND R> =
  IF  SWAP COUNT TYPE TAB  .ST?  ELSE  ." ??" 2DROP  THEN ;
\ .ESC                                                 18dec93py
CREATE FOPBTABLE
00 FLT, ADD     01 FLT, MUL     02 FLT, COM     03 FLT, COMP
04 FLT, SUB     05 FLT, SUBR    06 FLT, DIV     07 FLT, DIVR
08 FLT, LD      09 FLT, XCH     0A FLT, ST      0B FLT, STP
CREATE "FPTRS ," SFLOATDWORD DFLOATWORD  "      6 "FPTRS C!
: .MODST  COUNT TYPE DUP 200 AND IF  ." P"  THEN  TAB
  DUP 400 AND IF  DUP 7 AND .ST .,  THEN  0 .ST
  DUP 400 AND 0= IF  DUP 7 AND ., .ST  THEN  DROP ;
: .FMODM  OVER 9 RSHIFT DUP >R 1 AND IF ." I" THEN  COUNT TYPE TAB
  R> "FPTRS *." ."  PTR " FF AND .ADDR ;
: .MODFB ( IP opcode -- IP' )  DUP 1D0 = IF DROP ." nop" EXIT THEN
  DUP 7F8 AND 5C0 = IF ." free" TAB 7 AND .ST EXIT  THEN
  DUP DUP 38 AND 3 RSHIFT SWAP 100 AND 5 RSHIFT OR >R FOPBTABLE
  BEGIN  COUNT 1F AND R@ <  WHILE  COUNT +  REPEAT  RDROP
  OVER C0 AND C0 =  IF  .MODST  ELSE  .FMODM  THEN  ;
\ .ESC                                                 22may93py
CREATE FOPATABLE
00 FLT, LDENV   01 FLT, LDCW    02 FLT, STENV   03 FLT, STCW
                05 FLT, LD                      07 FLT, STP
08 FLT, RSTOR                   0A FLT, SAVE    0B FLT, STSW
0C FLT, BLD     0D FLT, ILD     0E FLT, BSTP    0F FLT, ISTP

: .MODFA  ( IP opcode -- IP' )
    DUP 7E0 = IF  DROP ." stsw" TAB ." AX" EXIT  THEN
    DUP 600 AND 7 RSHIFT OVER 18 AND 3 RSHIFT OR >R FOPATABLE
    BEGIN  COUNT 1F AND R@ <  WHILE  COUNT +  REPEAT
    DUP 1- C@ R> = 0= IF  DROP  C" ??"  THEN
    COUNT TYPE TAB FF AND .ADDR ;


\ .ESC                                                 02mar97py

: .FOP2   1F AND  DUP 2 = IF  DROP ." CLEX" EXIT  THEN
  DUP 3 = IF  DROP ." INIT" EXIT THEN ." ??" .  ;
: .ESC  ( ip -- ip' )  COUNT OPCODE @ 7 AND 8 LSHIFT OR
  DUP 7E0 AND 1E0 = IF  .FOP1  EXIT  THEN
  DUP 7E0 AND 3E0 = IF  .FOP2  EXIT  THEN
  DUP 120 AND 120 = IF  .MODFA EXIT  THEN  .MODFB ;

\ EXTRA STUFF FOR SSE

\ vex SSE codes (les/lds in 16/32 bit mode)

: VEX-REST ( byte -- )
	DUP 3 AND CASE
	    1 OF  LENGTH @ 1 XOR LENGTH !  ENDOF
	    2 OF  \ EQUALS F3 PREFIX, REPE
		20 REX +!  ENDOF
	    3 OF  \ EQUALS F2 PREFIX, REP
		10 REX +!  ENDOF
	ENDCASE
	DUP 4 AND 5 LSHIFT REX +!  100 REX +!
	3 RSHIFT 0F AND 0F XOR VVVV !
 ;

: .VEXC4 ( ip -- ip' )
	COUNT DUP 1F AND MAP-SELECT ! 5 RSHIFT 7 XOR 40 OR REX !
	COUNT DUP 4 RSHIFT 8 AND REX +!
 VEX-REST  'v' EMIT .0FCD  REX OFF ;

: .VEXC5 ( ip -- ip' )
	COUNT DUP 5 RSHIFT 4 AND 4 XOR 40 OR REX !
	1 MAP-SELECT !  VEX-REST  'v' EMIT .0FCD  REX OFF ;

\ .MMI                                                 02mar97py

: .MMR ( reg -- )  ." MM" 7 AND 0 .R ;
: .MMA ( r/m -- )  DUP $C0 <
  IF ." QUAD PTR " .ADDR  ELSE  .MMR  THEN ;
: .MMQ ( ip -- ip' )  TAB MOD@ .MMR ., .MMA ;
: .MMS ( -- )  OPCODE @ 3 AND S" bwdq" DROP + C@ EMIT ;
: .MMX ( ip -- ip' )  .MMS  .MMQ ;
: .MMI ( ip -- ip' )  MOD@ 2/ 3 AND
  S" ??rlrall" DROP SWAP 2* + 2 TYPE .MMS TAB .MMR ., .8B ;

\ SSE INSTRUCTIONS

: .S/P ( -- ) 'S' 'P' S? SELECT EMIT ;
: .S/D ( -- ) 'S' 'D' S? IF SS? ELSE LENGTH @ 0= THEN SELECT EMIT ;

: .SSE-SUFF ( -- ) .S/P .S/D ;
: .SSEREG ( n -- n ) 'y' 'x' L? SELECT EMIT ." mm" #.R ;

: .SSEADDR ( addr r/m -- addr' ) DUP 7 AND >R 6 RSHIFT
  DUP 3 =            IF  DROP R> >B? .SSEREG    EXIT  THEN
  DUP 0= R@ 5 = AND  IF  DROP RDROP .[ .32U .] EXIT  THEN
  R@  4 =            IF       RDROP    .SIB    EXIT  THEN
  CELLS .DISP + .PERFORM  R> .[ .SIB/REG .] ;

: .SSE ( ip -- ip' )
    .SSE-SUFF TAB MOD@ >R? .SSEREG .,
    VEX? IF VVVV @ .SSEREG ., THEN
    .SSEADDR ;
: .SSEA ( ip -- ip' )
    .SSE-SUFF TAB MOD@ >R? >R .SSEADDR ., R> .SSEREG
    VEX? IF ., VVVV @ .SSEREG  THEN ;
: .CVT ( ip -- ip' ) .S/P ." i2" .SSE-SUFF TAB MOD@ >R? .SSEREG .,
    VEX? IF VVVV @ .SSEREG ., THEN
    .SSEADDR ;
: .CVTA ( ip -- ip' ) .SSE-SUFF '2' EMIT .S/P 'I' EMIT TAB MOD@ >R?
    >R .SSEADDR ., R> .REG
    VEX? IF VVVV @ .SSEREG ., THEN ;
: .CVTB ( ip -- ip' ) .SSE-SUFF '2' EMIT .S/P 'I' EMIT TAB MOD@ >R?
    >R .SSEADDR ., R> .SSEREG
    VEX? IF VVVV @ .SSEREG ., THEN ;
: .SSEC ( ip -- ip' )
    .S/D TAB MOD@ >R? .SSEREG .,
    VEX? IF VVVV @ .SSEREG ., THEN
    .SSEADDR ;

: .CMOV ( ip -- ip' )
 //.  CODNAME.- .COND  .MODT ;

: .NOOP ?REX. CODNAME. ;

DEFER INST.

: $W.P ( addt -- addr+ )
 ?REX. CODNAME. WCOUNT ." $" .$DU ;

: .GR.P $48  REX OR!
 CODNAME.  OPCODE @ 7 AND .S/REG ;

: [T"]
  TO CODNAME
  OVER = IF  DROP 
\ + INLINE?  ." <|"  ['] INST. DEFER@ T_?.NAME>S ." >"
  INST. RDROP BREAK
;

: T"  POSTPONE C" POSTPONE [T"] ; IMMEDIATE

: BAD
  OVER XOR IF BREAK
	RDROP DROP 
	TAB ." .byte	0x" ?REX.X OPCODE C@ H.-
;


: .M0F
 COUNT  DUP OPCODE !

  ['] .MODT	TO INST.	02 T" lar"	03 T" lsl"
	AF T" imul"
	BC T" bsf"
	BD T" bsr"

  ['] .SSE	TO INST.
	10 T" mov"	28 T" mova"
	51 T" sqrt"	52 T" rsqrt"	53 T" rcp"	54 T" and"
	55 T" andn"	56 T" or"	57 T" xor"	58 T" add"
	59 T" mul"	5C T" sub"	5D T" min"	5E T" div"
	5F T" max"
  ['] .SSEA	TO INST.
	11 T" mov"	29 T" mova"	2B T" movnt"

  ['] .CVT	TO INST.	2A T" cvt"
  ['] .CVTA	TO INST.	2C T" cvtt"	2D T" cvt"
  ['] .SSEC	TO INST.	2E T" ucomis"	2F T" comis"
  ['] .MOVX	TO INST.	B6 T" movzb"	B7 T" movzw"
				BE T" movsb"	BF T" movsw"
  ['] .GRP8	TO INST.	BA T" bt"
  ['] .EV	TO INST.	C7 T" cmpxchg8b"
  ['] .MMQ	TO INST.
	D5 T" pmullw"	E5 T" pmulhw"	F5 T" pmaddwd"
	DB T" pand"	DF T" pandn"	EB T" por"
	EF T" pxor"
\ 0Ffld
  FE AND 

  DUP 00 = IF DROP .GRP6 BREAK
  ['] .SHD	TO INST.	A4 T" shld"	AC T" shrd"
  ['] .MODB	TO INST.	A6 T" cmpxchg"	C0 T" xadd"

  FC AND 

  ['] .MMI	TO INST.	70 T" ps"
  ['] .MMX	TO INST.

	D0 T" psrl"	D8 T" psubu"
	E0 T" psra"	E8 T" psubs"
	F0 T" psll"	F8 T" psub"
	DC T" paddu"	EC T" padds"
	FC T" padd"

  F8 AND 
  ['] .MOVRX	TO INST.	20 T" mov"
  ['] .LXS	TO INST.	B0 T" l"
  F0 AND 
  ['] .CMOV	TO INST.	40 T" cmov"
  ['] .JL	TO INST.	80 T" j"
  ['] .SET	TO INST.	90 T" set"

\ 0Ffld
  DROP  OPCODE @
  F7 AND
  ['] .PSEG	TO INST.	A0 T" push"	A1 T" pop"
  E7 AND
  ['] .BT	TO INST.	A3 T" bt"                

\             00 00 T, .NOOP 0F???"
 ?REX. DROP 1- TAB ." .byte	0x"  ?REX.X 'F' EMIT

 ;

' .M0F IS .0FCD

: .MCODE  ( addr -- addr+)
  DUP L@   $fa1e0ff3 =
  IF  ."	endbr64" 4 +
  BREAK

  WCOUNT
  ['] CODNAME. TO INST.
	060F T" clts"	080F T" invd"
	090F T" wbinvd"	300F T" wrmsr"
 320F T" rdmsr"

 9848 T" cltq"	9948 T" cqto"  CB48 T" lretq"

 F8FF AND
 DUP b848 = IF	SWAP ." 	movabs	$" DUP @ .$DS 8 + .,
		SWAP 8 >> 7 AND ." %r" "REGS *." BREAK

  DROP 2- COUNT
  ['] CODNAME. TO INST.

	6c T" insb	(%dx),%es:(%rdi)"
	6d T" insl	(%dx),%es:(%rdi)"
	6e T" outsb	%ds:(%rsi),(%dx)"
	6f T" outsl	%ds:(%rsi),(%dx)"
	82 BAD
	90 T" nop"	98 T" cwtl"	99 T" cqto"	9B T" wait"
	9C T" pushf"	9D T" popf"	9E T" sahf"	9F T" lahf"

	a4 T" movsb	%ds:(%rsi),%es:(%rdi)"
	a5 T" movsl	%ds:(%rsi),%es:(%rdi)"
	a6 T" cmpsb	%es:(%rdi),%ds:(%rsi)"
	a7 T" cmpsl	%es:(%rdi),%ds:(%rsi)"
	aa T" stos	%al,%es:(%rdi)"
	ab T" stos	%eax,%es:(%rdi)"
	ac T" lods	%ds:(%rsi),%al"
	ad T" lods	%ds:(%rsi),%eax"
	AE T" scas	%es:(%rdi),%al"
	AF T" scas	%es:(%rdi),%eax"

	ec T" in     	(%dx),%al"
	ed T" in	(%dx),%eax"
	ee T" out	%al,(%dx)"
	ef T" out	%eax,(%dx)"

	C3 T" ret"	C9 T" leave"
	F0 T" lock"	F2 T" repnz"	F3 T" repz"

\+ INLINE? CB T" lret" \ Warning
	CC T" int3"
\+ INLINE? CF T" iret" \ Warning

 D6 T" insl"	D7 T" xlat"
 F4 T" hlt"	F5 T" cmc"

  DUP OPCODE !

  DUP 0F = IF DROP .0FCD BREAK

  DUP 64 = IF DROP  OPCODE @ 1 AND 4 + SEG: ! RECURSE BREAK
  DUP 66 = IF DROP  LENGTH @ 1 XOR  LENGTH ! RECURSE BREAK
  DUP 67 = IF DROP ALENGTH @ INVERT ALENGTH ! RECURSE BREAK

  ['] .MOVDX	TO INST. 63 T" movslq"
  ['] .IV	TO INST. 68 T" push"
  ['] .MODIV	TO INST. 69 T" imul"
  ['] .IB	TO INST. 6A T" push"
  ['] .MODIB	TO INST. 6B T" imul"

  OVER C@ BD U<  IF
  ['] .MODT	TO INST. 8D T" lea"
  THEN

  OVER C@
DUP 17 ANDC
SWAP 45 <> AND
 0= IF
  ['] .EV	TO INST. 8F T" pop"
  THEN
\  ['] .FAR	TO INST. 9A T" callf" EA T" jmpf"
  ['] $W.P	TO INST. C2 T" ret"


\  DUP C4 = IF DROP .VEXC4 BREAK
\  DUP C5 = IF DROP .VEXC5 BREAK

  ['] .ENTER	TO INST. C8 T" enter"          
  ['] .IB	TO INST. CD T" int"

  ['] .JB	TO INST. E0 T" loopne" E1 T" loope"
	E2 T" loop"	EB T" jmp"

\  ['] .IO#	TO INST.	E4 T" in"	E6 T" out"
  DUP e4 = IF DROP ."	in	$0x" COUNT H.- ." ,%al"		BREAK
  DUP e5 = IF DROP ."	in	$0x" COUNT H.- ." ,%eax"	BREAK
  DUP e6 = IF DROP ."	out	%al,$0x" COUNT H.-		BREAK
  DUP e7 = IF DROP ."	out	%eax,$0x" COUNT H.-		BREAK
  
  ['] .JV	TO INST. E8 T" call" 
  ['] .JM	TO INST. E9 T" jmp"

  FE AND 

  ['] .MODB	TO INST.	84 T" test" 86 T" xchg"          
  ['] .IGR	TO INST.	A8 T" test"

  DUP C0 = IF DROP .GRP2I BREAK
  ['] .RET	TO INST.	C2 T" ret" CA T" retf"
  ['] .MOVI	TO INST.	C6 T" mov"

  DUP F6 = IF DROP .GRP3 BREAK
  DUP FE = IF DROP .GRP4 BREAK

  FC AND
  DUP 80 = IF DROP .GRP1 BREAK
  DUP D0 = IF DROP .GRP2 BREAK
  ['] .MOVO	TO INST.	A0 T" movabs"

  ['] .ARI	TO INST.	88 T" mov"
  F8 AND 
	00 T" add"	08 T" or"	10 T" adc"	18 T" sbb"
	20 T" and"	28 T" sub"	30 T" xor"	38 T" cmp"

  ['] .GR.P TO INST.  50 T" Push" 58 T" pop" C8 T" bswap"

  ['] .XCHA	TO INST.	90 T" xchg"
  ['] .IGRB	TO INST.	B0 T" mov"
  ['] .IGRV	TO INST.	B8 T" mov"
\  ['] .ESC	TO INST.	D8 T" f"

  DUP F8 = IF DROP .STCL BREAK

  F0 AND 

  DUP 40 =
  IF DROP OPCODE @ REX ! RECURSE BREAK

  DUP 70 = IF DROP TAB ." j" .COND .JB- BREAK
  
  DROP  OPCODE @

  DROP  OPCODE @
  FD AND
  ['] .MOVS	TO INST.	8C T" mov"

  DROP  OPCODE @
  E7 AND
  ['] .PSEG	TO INST.	06 T" push"	07 T" pop"
  DUP 26 = IF DROP  OPCODE @ 3 RSHIFT 3 AND SEG: ! RECURSE BREAK
  DUP 27 = IF DROP .ADJ BREAK

  DROP TAB ." .byte	0x" ?REX.X OPCODE C@ H.-
;

' .MCODE TO .CODE

 BASE !
EXPORT

0 VALUE NEXT-INST

: INST  ( ADR -- ADR' )
        DUP TO NEXT-INST
  DUP   REX OFF LEN! .MCODE \ disline
  DUP ROT TAB ." # " DUP H.
  //WAS
  IF  TAB  ." */.byte 0x"  COUNT DUP H.-
	$BB = \ drop 0
	IF
 DUP  CR TAB   ." .long " L@ .$DS 4 +
 2DUP <> IF
 CR TAB  ." .byte 0x"  COUNT H.-
 ?DO ." ,0x" I C@  H.- LOOP
	 THEN

	ELSE  ?DO ." ,0x" I C@  H.- LOOP
	THEN  0 TO //WAS
  ELSE     ?DO	I C@ H.  LOOP
  THEN
   STRBYTE?
  IF CR TAB ." .byte 0x"  COUNT DUP H.-
 	0 ?DO ." ,0x" COUNT  H.- LOOP

	0 TO STRBYTE?
  THEN
  DOVECT?
  IF   CR TAB ." .quad "  DUP @ ?.NAME>S 8 +	0 TO DOVECT?
  THEN

  XDO?
  IF  1+ CR TAB ." push	$"  DUP L@ ?.NAME>S 4 +	0 TO XDO?
  THEN
;

: DISA ( assr - addr )
 BEGIN
   CR INST
   KEY $20 OR 'q' =  \ Esc
 UNTIL
;
 
: MINST_ ( [INST] -- [INST'] )
  DUP  C@ 9 EMIT ." .inst 0x"  H.- 1+ ;
 
\- H.H : H.H C@ 2 H.N SPACE ;

: SMINST ( [INST] -- [INST+x] )
\    DUP H.
	DUP NEAR_NFA 
	>R DUP
	IF   DUP NAME>  R> - 0=
	     IF \ CR TAB	   DUP ." .global " MCOUNT TYPE 
		   CR DUP MCOUNT TYPE ." :"
	     THEN DROP
	ELSE RDROP DROP
	THEN

  BASE @ >R DECIMAL
  CR INST

  R> BASE !
 ;

' SMINST ->DEFER MINST

 VARIABLE  COUNT-LINE

: REST          ( ADR -- )
                20    COUNT-LINE !
\+ MAX_REFERENCE       0 TO MAX_REFERENCE
                DUP TO NEXT-INST
                BEGIN
                        CR
                        NEXT-INST C@
                        DUP  0xC3 <> 
                        SWAP 0xE9 <> AND    \ NEXT, BEHIND US?
\+ MAX_REFERENCE          NEXT-INST MAX_REFERENCE U< OR
                        OVER THERE - 0x100 U> AND
                WHILE   INST
                        COUNT-LINE @ 1- DUP 0=  \ SEE-KET-FL AND
                           IF 9 EMIT ." \ Press <enter> | q | any" KEY UPC
                            DUP   0xD = IF 2DROP 1  ELSE
                              DUP [CHAR] Q = SWAP 0x1B =
                              OR IF  2DROP CR EXIT    THEN
                                DROP 20    THEN
                           THEN
                        COUNT-LINE !
                REPEAT  DROP ." END-CODE  "
                ;

: SEE       ( -- )
            ' REST ;

;MODULE


