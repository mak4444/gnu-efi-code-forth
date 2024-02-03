
: A7\ POSTPONE \ ; IMMEDIATE
: A9\ ; IMMEDIATE
: MC! C! ;
: MC@ C@ ;
: MW! W! ;
: MW@ W@ ;
:  M!  ! ;
:  M@  @ ; 
: M0! 0! ;
: M+! +! ;
:  M+  + ; 
: M-/ / ;
: MNEGATE NEGATE ; 
: MOVER OVER ;
: MAND AND ;
: MCR CR ;
: MH. H. ;
: MHERER H. ;
: M_. . ;
: M.S .S ;
: MDEPTH DEPTH ;
: MHEX HEX ;
: MKEY KEY ;
\+ KEY? : MKEY? KEY? ;
: MEXECUTE EXECUTE ;
: MWDS WORDS ;
: MCMOVE CMOVE ;
: MFILL FILL ;
: \ZEOF ;
: M\EOF \EOF ;
\ : MCHAR CHAR ;
\- ANDC : ANDC INVERT AND ;

: SYNONYM  HERE >R DP ! HEADER R> DP ! ;

$804040e8 VALUE 'FSTART
'FSTART $800000 + VALUE 'FGLOB
$90000000 VALUE 'FDATA

: MREAD-FILE READ-FILE ;

\- BREAK : BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE

[IFNDEF] /*
: /*  ( -- )
  BEGIN
    PARSE-NAME DUP 0=
    IF  NIP  REFILL   0= IF DROP TRUE THEN
    ELSE  S" */" COMPARE 0=  THEN
  UNTIL
; IMMEDIATE
[THEN]

0 VALUE SSBB

\- DUPCOUTSET : DUPCOUTSET DUP TO H-STDOUT ;
\- COUTRESTORE : COUTRESTORE ( H-STDOUT CLOSE-FILE DROP)  1 TO H-STDOUT ;


\ REQUIRE <SIGN>	lib/include/float0.f

VECT	LOADER_SYNCHRONIZE
VECT	LOADER_LOAD
VECT	LOADER_GO

0 VALUE THS_ID
: THS_CREATE
    R/W CREATE-FILE THROW TO THS_ID
;

: THS_CLOSE
  THS_ID 0= IF BREAK
  THS_ID   CLOSE-FILE THROW 
 0 TO THS_ID
;

: THS>
  THS_ID DUPCOUTSET DROP INTERPRET
 COUTRESTORE
;

: DO_THS ( cfa -- )
   THS_ID 0= IF DROP BREAK
  THS_ID DUPCOUTSET DROP  EXECUTE
 COUTRESTORE
;
[IFNDEF] CHAR-UPPERCASE
: CHAR-UPPERCASE ( c -- c1 )
  DUP [CHAR] a [CHAR] z 1+ WITHIN IF 32 - EXIT THEN
;
[THEN]

: UP_TYPE ( adr len  -- )
  0 ?DO COUNT
  DUP  '"' = IF '\'  EMIT THEN
  DUP  '\' = IF '\'  EMIT THEN
 CHAR-UPPERCASE EMIT
	 LOOP DROP ;

0 VALUE TLASTFLG

: $THS (  adr len -- )
   THS_ID 0= IF 2DROP BREAK
 
 ['] TYPE DO_THS
;

: $ETHS (  c -- )
   THS_ID 0= IF 2DROP BREAK
 ['] EMIT DO_THS
;

: $UPTHS (  adr len -- )
   THS_ID 0= IF 2DROP BREAK
 ['] UP_TYPE DO_THS
;

VECT VGTYPE

' UP_TYPE TO VGTYPE

: $:THS ( adr len -- )
   THS_ID 0= IF 2DROP BREAK

   BL $ETHS   TLASTFLG 0 (D.)  ['] TYPE DO_THS   0 TO TLASTFLG  ['] CR DO_THS

 $THS
 '"' $ETHS
  >IN M@ PARSE-NAME $UPTHS >IN M!
 '"' $ETHS
 BL $ETHS
  >IN M@ PARSE-NAME  ['] VGTYPE DO_THS >IN M!

\   ['] SPACE DO_THS
 BL $ETHS

 S" 0x" $THS  CURSTR @ ['] H. DO_THS \ 0 (D.)  ['] TYPE DO_THS 
\  CURSTR @ 0 (D.)  ['] TYPE DO_THS 

 ;


: $:THS_t ( adr len -- )
   THS_ID 0= IF 2DROP BREAK

   BL $ETHS   TLASTFLG 0 (D.)  ['] TYPE DO_THS   0 TO TLASTFLG  ['] CR DO_THS
 $THS
 '"' $ETHS
  >IN M@ PARSE-NAME $UPTHS >IN M!
 '"' $ETHS
 BL $ETHS
  >IN M@ PARSE-NAME  ['] VGTYPE DO_THS >IN M!

\   ['] SPACE DO_THS
 BL $ETHS

 S" 0x" $THS  CURSTR @ ['] H. DO_THS \ 0 (D.)  ['] TYPE DO_THS 
\  CURSTR @ 0 (D.)  ['] TYPE DO_THS 

 ;

: C$:THS ( n adr len -- n )
   THS_ID 0= IF 2DROP BREAK

   BL $ETHS   TLASTFLG 0 (D.)  ['] TYPE DO_THS   0 TO TLASTFLG  ['] CR DO_THS

 $THS
 '"' $ETHS
  >IN M@ PARSE-NAME $UPTHS >IN M!
 '"' $ETHS
 BL $ETHS
DUP
   DUP  'FSTART - 0xFFFFFF U<
  IF  'FSTART -  S" fimg+" $THS
  THEN
  	'0' $ETHS 'x' $ETHS
	  ['] H. DO_THS \ 0 (D.)  ['] TYPE DO_THS 
\   ['] SPACE DO_THS
 BL $ETHS

 S" 0x" $THS  CURSTR @ ['] H. DO_THS \ 0 (D.)  ['] TYPE DO_THS 

 ;



: :#THS		S" Wortbirne "	$:THS ;
: VAR:THS	S" VARIABLE_W "	C$:THS ;
: CON:THS	S" CONSTANT_W "	C$:THS ;

: THS::
   BL $ETHS   TLASTFLG 0 (D.)  ['] TYPE DO_THS 0 TO TLASTFLG  ['] CR DO_THS
  S" S_VIEW_PATH B:\xx" $THS  PARSE-NAME $THS  ['] CR DO_THS
\  S" VIEW_S 0" $THS
  ['] CR DO_THS ['] CR DO_THS
 ;

VECT G_INCLUDED

: GINCLUDED
  2DUP 2>R
     G_INCLUDED 
   BL $ETHS  TLASTFLG 0 (D.)  ['] TYPE DO_THS 0 TO TLASTFLG  ['] CR DO_THS
\  S" S_VIEW_PATH " $THS  2R> OVER C@ '.' = IF $f /STRING THEN  $THS
  S" S_VIEW_PATH ForthSrc/" $THS  2R> $THS
  ['] CR DO_THS ['] CR DO_THS
  S" VIEW_S" $THS

;


\ REQUIRE MFFFFF	~mak/lib/THERE/tcfloat.f
