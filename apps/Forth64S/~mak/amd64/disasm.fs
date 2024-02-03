\ *** Disassembler for amd64 ***

\ Copyright (C) 1992-2000 by Bernd Paysan (486 disassemlber)
\ You should have received a copy of the GNU General Public License
\ along with this program. If not, see http://www.gnu.org/licenses/.

\
\ amd disassembler loadscreen                         19may97py

REQUIRE NUMBER? ~mak/lib/fpcnum.f

REQUIRE CASE lib/ext/case.f

[IFNDEF] \+
: \+    POSTPONE [DEFINED] 0= IF POSTPONE \ THEN ; IMMEDIATE
: \-    POSTPONE [DEFINED]    IF POSTPONE \ THEN ; IMMEDIATE
[THEN]      

\+ CASE-INS CASE-INS ON
\- HEX. : HEX. H. ;
\- \G : \G POSTPONE \ ; IMMEDIATE

[IFNDEF] :NONAME
: :NONAME ( C: -- colon-sys ) ( S: -- xt ) \ 94 CORE EXT
  LATEST ?DUP IF 1+ C@ C-SMUDGE C! SMUDGE THEN
  HERE DUP TO LAST-NON [COMPILE] ]
;
[THEN]      
\- endif : endif POSTPONE THEN ; IMMEDIATE
\- 2, : 2, , , ;
\- DEFER : DEFER VECT ;
\+ ALIGN-BYTES 8 ALIGN-BYTES !

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


: D# BASE @ >R DECIMAL PARSE-WORD EVALUATE R> BASE ! ;  IMMEDIATE
\- BOUNDS : BOUNDS OVER + SWAP ;
\- #TAB 9 CONSTANT  #TAB
\- ," : ," '"' PARSE S", ;
\- <= : <= > 0= ;

\- SELECT : SELECT IF DROP BREAK NIP ;
\- SW@ : SW@ W@ $8000		XOR $8000 - ;
\- SL@ : SL@ L@ $80000000	XOR $80000000 - ;
\- IS  : IS POSTPONE TO ; IMMEDIATE

: ?.NAME      ( CFA -- )
\ ELIMINATE " 0x"
\	DUP H.  \ 1 H.R>S SSPACE
	NEAR_NFA 
	>R DUP
	IF ."  ( " DUP COUNT TYPE 
	     NAME>  R> - DUP
	     IF   DUP ." +" NEGATE H. \ >S
	     THEN DROP        ."  ) "
	ELSE RDROP DROP
	THEN ;

VOCABULARY DISASSEMBLER

ALSO DISASSEMBLER DEFINITIONS

 BASE @  $8 BASE !

VARIABLE CP?

\ long words and presigns                              31dec92py

: .#    '#' EMIT ;
: .$    '$' EMIT ;
: .,    ',' EMIT ;
: .+    '+' EMIT ;
: .-    '-' EMIT ;
: ..    '.' EMIT ;
: .:    ':' EMIT ;
: .[    '[' EMIT ;
: .]    ']' EMIT ;
: #.R ( n -- ) \ print decimal
    0 ['] .R D# 10 BASE-EXECUTE ;

\ signed / unsigned byte, word and long output         07aug10py


: .LFORMAT   ( addr -- )  $C U.R ." :" ;

: .$DU  ( N -- )       .$ H. ;
: .$DS  ( N -- )       DUP 0< IF  .- NEGATE  THEN  .$DU ;

: .HBY   ( 8B -- )     BASE @ >R HEX  0  <#  # #  #>  TYPE R> BASE ! ;
: .$BU  ( 8B -- )      .$ .HBY ;
: .$BS  ( 8B -- )      $FF AND DUP $7F >
                           IF .- $100 SWAP - THEN .$BU  ;

: .DUMP ( ADDR LEN -- )   BOUNDS DO  I C@ .HBY  LOOP ;

\ Variables and tabs                                   16nov97py

VARIABLE OPCODE
VARIABLE MODE
VARIABLE LENGTH
VARIABLE ALENGTH
VARIABLE .LENGTH
VARIABLE .ALENGTH
VARIABLE .AMD64MODE  .AMD64MODE ON
VARIABLE SEG: SEG: ON
VARIABLE REX
VARIABLE MAP-SELECT
VARIABLE VVVV

  D# 36 CONSTANT  BYTFLD
  D# 10 CONSTANT  MNEFLD
  D# 18 CONSTANT  ADDRFLD
: TAB #TAB EMIT ;

: LEN!  .LENGTH @ LENGTH !  .ALENGTH @ ALENGTH !  SEG: ON ;

: T,   SWAP ALIGN C, C, ALIGN ' ,   '"' PARSE HERE OVER 1+ ALLOT PLACE ALIGN ;

\ Strings                                              07feb93py
CREATE "REGS  ," axcxdxbxspbpsidi8 9 101112131415"
CREATE "BREG  ," al  cl  dl  bl  ah  ch  dh  bh  r8l r9l r10lr11lr12lr13lr14lr15l"
CREATE "BREG2 ," al  cl  dl  bl  spl bpl sil dil r8l r9l r10lr11lr12lr13lr14lr15l"
CREATE "16RI ," bx+sibx+dibp+sibp+disi   di   bp   bx   "
CREATE "PTRS ," dwordword byte "
CREATE "IDX  ,"   *2*4*8"
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
: P? ( -- FLAG ) 100 REX? ;
: W? ( -- FLAG )  10 REX? ;
: SS? ( -- FLAG ) D# 20 REX? ;
: SD? ( -- FLAG ) D# 10 REX? ;
: S? ( -- FLAG ) D# 30 REX? ;
: L? ( -- FLAG ) D# 80 REX? ;
: VEX? ( -- FLAG ) $100 REX? ;
: >R? ( REG -- REG' )   4 REX? 10 AND OR ;
: >X? ( REG -- REG' )   2 REX? 10 AND OR ;
: >B? ( REG -- REG' )   1 REX? 10 AND OR ;

\ Register display                                     05dec92py

: *."  ( n addr -- )  COUNT >R SWAP R@ * + R> -TRAILING TYPE ;
: .REGSIZE ( reg -- reg )
    DUP 7 <= IF  'r' 'e' W? SELECT EMIT  ELSE  'r' EMIT  THEN  ;
: .(REG ( n l -- )
    >R R@ 0= IF  .REGSIZE "REGS ( " )  ELSE  R@ 2 = IF
	    "BREG2 "BREG P? SELECT  ELSE  "REGS ( " ) THEN  THEN
    OVER >R *." R> 7 > IF  CASE R@
	    0 OF  W? 0= IF 'D' EMIT  THEN  ENDOF
	    1 OF  'W' EMIT                 ENDOF
	    2 OF  'B' EMIT                 ENDOF
	ENDCASE  THEN  RDROP ;
: .REG  ( N -- )  LENGTH @ .(REG ;
: .R/REG  ( N -- )    >R? LENGTH @ .(REG ;
: .M/REG  ( N -- )    >X? LENGTH @ .(REG ;
: .EREG ( N -- )  .REGSIZE "REGS *." ;
: .MI/REG  ( N -- )   >X? .EREG ;
: .SIB/REG  ( N -- )  >B? .EREG ;
: .S/REG  ( N -- )  >B? LENGTH @ .(REG ;
: .SEG  ( N -- )  "SEG *." ;

: MOD@ ( addr -- addr' r/m reg )
  COUNT DUP 70 AND 3 RSHIFT SWAP 307 AND SWAP ;
: .8B  ( addr -- addr' )  COUNT .$BS ;
: .32B ( addr -- addr' )  DUP L@  .$DS 4 + ;
: .32U ( addr -- addr' )  DUP SL@  OVER +  .$DU 4 + ;
: .64B ( addr -- addr' )  DUP @   .$DS $8 + ;
: .64U ( addr -- addr' )  DUP @   .$DU $8 + ;

\ Register display                                     05dec92py

CREATE .DISP ' NOOP ,  ' .8B ,   ' .32B ,

: .SIB  ( addr mod -- addr' ) >R COUNT  DUP 7 AND 5 = R@ 0= AND
  IF    RDROP >R .32B R>
  ELSE  SWAP R> CELLS .DISP + PERFORM SWAP DUP 7 AND .[ .SIB/REG .]
  THEN  3 RSHIFT DUP 7 AND 4 = 0=
  IF    .[ DUP 7 AND .MI/REG 3 RSHIFT "IDX *." .]
  ELSE  DROP  THEN ;

: .32A  ( addr r/m -- addr' ) DUP 7 AND >R 6 RSHIFT
  DUP 3 =            IF  DROP R>       .S/REG    EXIT  THEN
  DUP 0= R@ 5 = AND  IF  DROP RDROP .[ .32U .] EXIT  THEN
  R@  4 =            IF       RDROP    .SIB    EXIT  THEN
  CELLS .DISP + PERFORM  R> .[ .SIB/REG .] ;
\ Register display                                     29may10py

: WCOUNT ( addr -- addr' w ) DUP W@ >R 2 + R> ;
: WXCOUNT ( addr -- addr' w ) DUP SW@ >R 2 + R> ;
: +8B  ( addr -- addr' )  COUNT  .$BS ;
: +16B ( addr -- addr' )  WCOUNT .$DS ;

CREATE .16DISP  ' NOOP , ' +8B , ' +16B ,

: .16R  ( reg -- ) .[ "16RI *." .] ;
: .16A  ( addr r/m -- addr' ) 307 AND
  DUP 006 =  IF  DROP WCOUNT .[ .$DU .] EXIT  THEN
  DUP 7 AND >R 6 RSHIFT  DUP 3 =  IF  DROP R> .M/REG EXIT  THEN
  CELLS .16DISP + PERFORM R> .16R  ;


\ Register display                                     01jan93py

: .ADDR ( addr r/m -- addr' )
  SEG: @ 0< 0= IF  SEG: @ .SEG ':' EMIT  THEN
  ALENGTH @  IF  .16A  ELSE  .32A  THEN ;

: .PTR  ( addr r/m -- addr' )
  DUP 300 < IF  LENGTH @ "PTRS *." ."  PTR "  THEN  .ADDR ;

: .MOD  ( addr -- addr' )  MOD@ .R/REG ., .ADDR ;
: .RMOD ( addr -- addr' )  MOD@ >R .ADDR R> ., .R/REG ;

: .IMM  ( addr -- addr' )  LENGTH @
    DUP 0= IF  DROP DUP L@  .$DS 4 + EXIT  THEN
  1 =    IF  WCOUNT .$DS EXIT  THEN  COUNT .$BS ;
: .IMM64  ( addr -- addr' )  LENGTH @
    DUP 0= IF  DROP DUP
	W? IF  @ .$DS CELL+  ELSE  L@  .$DS 4 +  THEN
	EXIT  THEN
  1 =    IF  WCOUNT .$DS EXIT  THEN  COUNT .$BS ;

\ .ari                                                 07feb93py

DEFER .CODE

: .B? ( -- ) OPCODE @ 1 AND 0= IF  2 LENGTH !  THEN ;
: .ARI   .B? TAB
  OPCODE @ DUP 4 AND  IF  DROP 0 .R/REG ., .IMM EXIT  THEN
  2 AND  IF  .MOD  ELSE  .RMOD  THEN ;
: .MODT  TAB .MOD ;
: .GR    TAB  OPCODE @ 7 AND .S/REG ;
: .REXINC  .AMD64MODE @ IF  OPCODE @ REX !  .CODE  REX OFF  EXIT  THEN
    ." inc" .GR ;
: .REXDEC  .AMD64MODE @ IF  OPCODE @ REX !  .CODE  REX OFF  EXIT  THEN
    ." dec" .GR ;

: .IGRV  .GR ., .IMM64 ;
: .IGRB  2 LENGTH ! .IGRV ;
: .IGR   .B? .GR ., .IMM ;
: .MODB  .B? TAB .RMOD ;

: .XCHA  .GR ., 0 .M/REG ;
\ .conds modifier                                      29may10py

: .COND ( -- ) OPCODE @
  17 AND  DUP 1 AND  IF  'N' EMIT  THEN  2/ "JMP *." ;
: .JB   TAB COUNT DUP $80 AND IF -$80 OR THEN OVER + .$DU ;
: .JV   TAB  ALENGTH @  IF  WXCOUNT OVER
  ELSE  DUP SL@ SWAP 4 +  TUCK  THEN  + DUP .$DU ?.NAME ;
: .JS   .COND .JB ;
: .JL   .COND .JV ;
: .SET  .COND TAB MOD@ DROP 2 LENGTH ! .PTR ;

: ASIZE   ALENGTH @ INVERT ALENGTH ! .CODE ;
: OSIZE   LENGTH @ 1 XOR   LENGTH ! .CODE ;
: .SEG:   OPCODE @ 3 RSHIFT 3 AND SEG: ! .CODE ;
: .SEGX   OPCODE @ 1 AND 4 +   SEG: ! .CODE ;
: .PSEG   TAB OPCODE @ 3 RSHIFT 7 AND .SEG ;
\ .GRP1 .GRP4 .ADJ .ARPL                               05dec92py
: .GRP1   .B? MOD@ GRP1 *." TAB .PTR .,
  OPCODE @ 3 AND 3 = IF  2 LENGTH !  THEN  .IMM ;
: .GRP2   .B? MOD@ $8 + GRP1 *." TAB .PTR .,
  OPCODE @ 2 AND IF ." CL" ELSE ." 1" THEN ;
: .GRP3   .B? MOD@ DUP >R GRP3 *." TAB
  R@ 3 > IF  0 .R/REG .,  THEN
  R@ 2 4 WITHIN  IF  .PTR  ELSE  .ADDR  THEN
  R> 2 < IF  ., .IMM  THEN ;
: .GRP4   .B? MOD@ DUP GRP4 *." TAB
  2 + 7 AND 4 < IF  .PTR  ELSE  .ADDR  THEN ;
: .ADJ    OPCODE @ DUP D# 10 AND
  IF  'a'  ELSE  'd'  THEN  EMIT  'a' EMIT  $8 AND
  IF  's'  ELSE  'a'  THEN  EMIT ;
: .SEG#   .[ DUP ALENGTH @ 2* 4 + + WCOUNT .$DU
  .: SWAP ALENGTH @ IF  WCOUNT .$DU  ELSE  .32U  THEN .] DROP ;
\ .MOVO .MOVX .STR                                     23jan93py
: .MOVO   TAB .B?
  OPCODE @ 2 AND 0= IF  0 .R/REG .,  THEN  $05 ALENGTH @ - .ADDR
  OPCODE @ 2 AND    IF  ., 0 .R/REG  THEN ;
: .MOVX   TAB MOD@ .R/REG ., 1 LENGTH ! .B? .PTR ;
: .MOVDX   TAB MOD@ .R/REG ., 0 LENGTH ! .B? .PTR ;
: .MOVI   .B? TAB MOD@ DROP .PTR ., .IMM ;
: .MOVS   TAB MOD@  OPCODE @ 2 AND
  IF  .SEG ., .ADDR  ELSE  >R .ADDR ., R> .SEG  THEN ;
: .STR    .B? C" DWB" 1+ LENGTH @ + C@ EMIT ;
: .FAR    TAB .SEG# ;
: .MODIV   .MODT ., .IMM ;
: .MODIB   .MODT ., 2 LENGTH ! .IMM ;
: .IV     TAB .IMM ;
: .IB     2 LENGTH ! .IV ;
: .EV     TAB MOD@ DROP .PTR ;
: .ARPL
    .AMD64MODE @ IF  0 LENGTH ! ." MOVSX" .MOVDX
    ELSE  ." ARPL" TAB  1 LENGTH !  .RMOD  THEN ;
\ .MNE                                                 16nov97py

: .IO   TAB .B? 0 .R/REG ., 1 LENGTH ! 2 .M/REG ;
: .IO#  TAB .B? 0 .R/REG ., COUNT .$BU ;
: .RET  OPCODE @ 1 AND 0= IF TAB WCOUNT .$DU THEN ;
: .ENTER  TAB WCOUNT .$DU ., COUNT .$BU ;
: .STCL OPCODE @ 1 AND IF ." st" ELSE ." cl" THEN
  S" CID " DROP OPCODE @ 2/ 3 AND + C@ EMIT ;


: .MNE ( addr field -- addr' )
  >R COUNT DUP OPCODE ! R>
    BEGIN  2DUP C@  AND  OVER 1+ C@ = 0= WHILE
	    CELL+ CELL+ COUNT + 7 + 7 INVERT AND
 REPEAT
  NIP DUP CELL+ CELL+  COUNT TYPE  CELL+ PERFORM LEN!
 ;

0 VALUE MNTBL
0 VALUE 0FTBL
:NONAME  MNTBL .MNE ; IS .CODE
: .0F     0FTBL  .MNE ;

\ .GRP6 .GRP7                                          07aug10py

: .GRP6  1 LENGTH ! MOD@ OPCODE @ 3 LSHIFT + GRP6 *." TAB .ADDR ;
: .GRP2I  .B? MOD@ $8 + GRP1 *." TAB .PTR ., 2 LENGTH ! .IMM ;
: .GRP8   MOD@ GRP8 *." TAB .ADDR ., 2 LENGTH ! .IMM ;
: .BT     OPCODE @ 3 RSHIFT 7 AND GRP8 *." TAB .RMOD ;
CREATE  LBSWAP  0 C, 3 C, 3 C, 0 C,
: .MOVRX  TAB  OPCODE @ DUP 3 AND LBSWAP + C@ XOR 7 AND >R
  MOD@ R@ 1 AND  IF  SWAP 7 AND .R/REG .,  THEN
  R@ 2/ C" CDT?" + 1+ C@ SWAP 0 <# # 'R' HOLD ROT HOLD #> TYPE
  R> 1 AND  0= IF  ., 7 AND .M/REG  THEN ;
: .LXS  OPCODE @ 7 AND "SEG1 *." .MODT ;
: .SHD  TAB .RMOD ., 2 LENGTH ! OPCODE @ 1 AND
  IF  1 .R/REG  ELSE  .IMM  THEN ;


\ .ESC                                                 22may93py
: FLT,  C, BL PARSE HERE OVER 1+ ALLOT PLACE ;
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

: .REP ( ip -- ip' )
    DUP C@ $F0 AND $40 = IF
	DUP 1+ C@ 0F = IF
	    COUNT  10 + REX !  .CODE  EXIT
	ELSE  DUP C@ 0F = IF  10 REX !  .CODE  EXIT  THEN  THEN  THEN
    ." rep " .CODE  ;
: .REPE ( ip -- ip' )
    DUP C@ $F0 AND $40 = IF
	DUP 1+ C@ 0F = IF
	    COUNT  20 + REX !  .CODE  EXIT
	ELSE  DUP C@ 0F = IF  20 REX !  .CODE  EXIT  THEN  THEN  THEN
    ." repe " .CODE  ;

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
	DUP 3 RSHIFT 0F AND 0F XOR VVVV ! ;

: .VEXC4 ( ip -- ip' )
    .AMD64MODE @ IF
	COUNT DUP 1F AND MAP-SELECT ! 5 RSHIFT 7 XOR 40 OR REX !
	COUNT DUP 4 RSHIFT 8 AND REX +! VEX-REST  'v' EMIT .0F  REX OFF
    ELSE  ." les" .MODT  THEN ;

: .VEXC5 ( ip -- ip' )
    .AMD64MODE @ IF
	COUNT DUP 5 RSHIFT 4 AND 4 XOR 40 OR REX !
	1 MAP-SELECT !  VEX-REST  'v' EMIT .0F  REX OFF
    ELSE  ." lds" .MODT  THEN ;

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
  CELLS .DISP + PERFORM  R> .[ .SIB/REG .] ;

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
    .COND .MODT ;

\ 0Ffld                                                16nov97py
ALIGN HERE TO 0FTBL
FE 00 T, .GRP6 "
FF 02 T, .MODT lar"             FF 03 T, .MODT lsl"
FF 06 T, NOOP clts"             F8 20 T, .MOVRX mov"
FF 08 T, NOOP invd"             FF 09 T, NOOP wbinvd"
FF 10 T, .SSE mov"              FF 11 T, .SSEA mov"
FF 28 T, .SSE mova"             FF 29 T, .SSEA mova"
FF 2A T, .CVT cvt"              FF 2B T, .SSEA movnt"
FF 2C T, .CVTA cvtt"            FF 2D T, .CVTA cvt"
FF 2E T, .SSEC ucomis"          FF 2F T, .SSEC comis"
F0 40 T, .CMOV cmov"            FF 51 T, .SSE sqrt"
FF 52 T, .SSE rsqrt"            FF 53 T, .SSE rcp"
FF 54 T, .SSE and"              FF 55 T, .SSE andn"
FF 56 T, .SSE or"               FF 57 T, .SSE xor"
FF 58 T, .SSE add"              FF 59 T, .SSE mul"
FF 5C T, .SSE sub"              FF 5D T, .SSE min"
FF 5E T, .SSE div"              FF 5F T, .SSE max"
F0 80 T, .JL j"                 F0 90 T, .SET set"
F7 A0 T, .PSEG push"            F7 A1 T, .PSEG pop"
FE A4 T, .SHD shld"             FE AC T, .SHD shrd"
E7 A3 T, .BT bt"                FE A6 T, .MODB cmpxchg"
FE B6 T, .MOVX movzx"           FF BA T, .GRP8 bt"
F8 B0 T, .LXS l"                FE BE T, .MOVX movsx"
FE C0 T, .MODB xadd"            F8 C8 T, .GR bswap"
FF AF T, .MODT imul"            FF BC T, .MODT bsf"
FF BD T, .MODT bsr"             FF C7 T, .EV cmpxchg8b"

\ 0Ffld                                                12apr98py

FC 70 T, .MMI ps"
FF 30 T, NOOP wrmsr"            FF 32 T, NOOP rdmsr"


FF D5 T, .MMQ pmullw"           FF E5 T, .MMQ pmulhw"
FF F5 T, .MMQ pmaddwd"
FF DB T, .MMQ pand"             FF $DF T, .MMQ pandn"
FF EB T, .MMQ por"              FF EF T, .MMQ pxor"
FC D0 T, .MMX psrl"             FC D8 T, .MMX psubu"
FC E0 T, .MMX psra"             FC E8 T, .MMX psubs"
FC F0 T, .MMX psll"             FC F8 T, .MMX psub"
FC DC T, .MMX paddu"            FC EC T, .MMX padds"
FC FC T, .MMX padd"             00 00 T, NOOP 0F???"

\ disassembler table                                   22may93py
ALIGN HERE TO MNTBL
FF 0F T, .0F "
E7 06 T, .PSEG push"            E7 07 T, .PSEG pop"
F8 00 T, .ARI add"              F8 08 T, .ARI or"
F8 10 T, .ARI adc"              F8 18 T, .ARI sbb"
E7 26 T, .SEG: "                E7 27 T, .ADJ "
F8 20 T, .ARI and"              F8 28 T, .ARI sub"
F8 30 T, .ARI xor"              F8 38 T, .ARI cmp"
F8 40 T, .REXINC "              F8 48 T, .REXDEC "
F8 50 T, .GR push"              F8 58 T, .GR pop"
FF 60 T, NOOP pusha"            FF 61 T, NOOP popa"
FF 62 T, .MODT bound"           FF 63 T, .ARPL "
FE 64 T, .SEGX "
FF 66 T, OSIZE "                FF 67 T, ASIZE "

\ disassembler table                                   21may94py

FF 68 T, .IV push"              FF 69 T, .MODIV imul"
FF 6A T, .IB push"              FF 6B T, .MODIB imul"
FE 6C T, .STR ins"              FE 6E T, .STR outs"
F0 70 T, .JS j"                 FF 82 T, NOOP ???"
FC 80 T, .GRP1 "                FE 84 T, .MODB test"
FE 86 T, .MODB xchg"            FC 88 T, .ARI mov"
FD 8C T, .MOVS mov"             FF 8D T, .MODT lea"
FF 8F T, .EV pop"
FF 90 T, NOOP nop"              F8 90 T, .XCHA xchg"
FF 98 T, NOOP cbw"              FF 99 T, NOOP cwd"
FF 9A T, .FAR callf"            FF 9B T, NOOP wait"
FF 9C T, NOOP pushf"            FF 9D T, NOOP popf"
FF 9E T, NOOP sahf"             FF 9F T, NOOP lahf"

\ disassembler table                                   22may93py

FC A0 T, .MOVO mov"             FE A4 T, .STR movs"
FE A6 T, .STR cmps"             FE A8 T, .IGR test"
FE AA T, .STR stos"             FE AC T, .STR lods"
FE AE T, .STR scas"
F8 B0 T, .IGRB mov"             F8 B8 T, .IGRV mov"
FE C0 T, .GRP2I "               FE C2 T, .RET ret"
FF C4 T, .VEXC4 "               FF C5 T, .VEXC5 "
FE C6 T, .MOVI mov"
FF C8 T, .ENTER enter"          FF C9 T, NOOP leave"
FE CA T, .RET retf"
FF CC T, NOOP int3"            FF 0CD T, .IB int"
FF CE T, NOOP into"             FF CF T, NOOP iret"


\ disassembler table                                   12aug00py
FC D0 T, .GRP2 "
FF D4 T, NOOP aam"              FF D5 T, NOOP aad"
FF D6 T, NOOP salc"
FF D7 T, NOOP xlat"             F8 D8 T, .ESC f"
FF E0 T, .JB loopne"            FF E1 T, .JB loope"
FF E2 T, .JB loop"              FF E3 T, .JB jcxz"
FE E4 T, .IO# in"               FE E6 T, .IO# out"
FF E8 T, .JV call"              FF E9 T, .JV jmp"
FF EA T, .FAR jmpf"             FF EB T, .JB jmp"
FE EC T, .IO in"                FE EE T, .IO out"
FF F0 T, .CODE lock "           FF F2 T, .REP "
FF F3 T, .REPE "                FF F4 T, NOOP hlt"
FF F5 T, NOOP cmc"              FE F6 T, .GRP3 "
FE FE T, .GRP4 "                F8 F8 T, .STCL "
00 00 T, NOOP ???"
\ addr! dis disw disline                               13may95py

: .86    1 .LENGTH !  .ALENGTH ON  LEN! ;
: .386   .LENGTH OFF  .ALENGTH OFF LEN! ;
: .AMD64 .386 .AMD64MODE ON ;

: INST  ( ADR -- ADR' )
  DUP CR ." ( "  HEX. ." ) "
  DUP .CODE \ disline
  DUP TAB ." \ "  ROT  ?DO	I C@ HEX.  LOOP
;

 BASE !

ALSO FORTH DEFINITIONS

: DISLINE ( ip -- ip' )
    DUP .LFORMAT TAB .CODE  ;

: DISASM ( addr u -- ) \ gforth
	OVER + >R
	BEGIN  DUP R@ U<  WHILE  CR DISLINE  REPEAT
	CR RDROP DROP ;

: DISP
 BEGIN
  INST
  KEY 0x20 OR 'q' =
 UNTIL
;

\+ DISCODE ' DISASM IS DISCODE

PREVIOUS PREVIOUS

