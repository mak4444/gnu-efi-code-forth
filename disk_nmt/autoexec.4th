.( autoexec.4th) CR

\- SYSTAB &SYSTAB @ CONSTANT SYSTAB
\- VOLUME &VOLUME @ CONSTANT VOLUME

\- REQUIRED : REQUIRED ( waddr wu laddr lu -- )
\- REQUIRED  2SWAP  SFIND  IF DROP 2DROP EXIT  ELSE 2DROP  INCLUDED THEN ;
\- REQUIRE : REQUIRE ( "word" "libpath" -- ) PARSE-NAME PARSE-NAME  REQUIRED ;
\- LAST-NON 0 VALUE LAST-NON
\- :NONAME : :NONAME ( C: -- colon-sys ) ( S: -- xt ) \ 94 CORE EXT
\- :NONAME  HERE DUP TO LAST-NON [COMPILE] ] ;
\- RECURSE : RECURSE \ Compile a call to the current (not yet finished) definition.
\- RECURSE	LAST @ NAME> LAST-NON UMAX COMPILE, ; IMMEDIATE

 REQUIRE VIEWS_SEARCH ForthLib\tools\defview.4th 

3 VALUE COLOR@
: S_COLOR!  DUP  [ ' COLOR! DEFER@ COMPILE, ] 
 TO COLOR@ ;

' S_COLOR! TO COLOR!

\- COLOR@ : COLOR@ TEXTOUTPUTMODE tm.Attribute C@ ;

\- EFI_BLUE   1 CONSTANT EFI_BLUE  
\- EFI_GREEN  2 CONSTANT EFI_GREEN 
\- EFI_RED    4 CONSTANT EFI_RED
\- EFI_BRIGHT 8 CONSTANT EFI_BRIGHT   
: >BG 4 << ; 3 COLOR! 

 REQUIRE EFI_ERROR_DO ForthLib\ext\error.4th 

\- SCFASET : SCFASET ( cfa adr len -- ) SNFAFIND DUP 0= IF -13 THROW THEN NAME>C ! ;
\- CFASET : CFASET  ( cfa <name> -- )  PARSE-NAME SCFASET ;
\- LABSET : LABSET  ( cfa -- )  HERE CFASET ;

\- \EOF : \EOF  ( -- )  BEGIN REFILL 0= UNTIL  POSTPONE \ ;

 ' KEY2 TO KEY
: 4FIELD 3 + 3 ANDC 4 FIELD ;
: 8FIELD 7 + 7 ANDC 8 FIELD ;
: *FIELD 8FIELD ;
: @FIELD $F + $F ANDC 8 FIELD ;

 REQUIRE SEE ForthLib\asm\disgasm.4th 
\+ F_?.NAME>S ' F_?.NAME>S  TO ?.NAME>S
 FLOAD ForthLib\tools\words.4th 
 REQUIRE CASE-INS ForthLib\lib\caseins.4th 
 REQUIRE STRUCTURES{ ForthLib\lib\structures.4th 

 FLOAD ForthLib\include\efidef.4th 
 FLOAD ForthLib\include\eficon.4th 
 FLOAD ForthLib\include\efiapi.4th 
 FLOAD ForthLib\include\efiprot.4th 

  SYSTAB ST*ConOut @ IO*Mode CONSTANT TEXTOUTPUTMODE


[IFDEF] GETMAXXY0
 GETMAXXY0
 [IF]  2DROP
 [ELSE] CONSTANT COLS CONSTANT ROWS
 [THEN]
[THEN]


\- ROWS 25 CONSTANT ROWS
\- COLS 80 CONSTANT COLS

1
[IF]

ROWS COLS * CONSTANT MON_SIZE
 
0  VALUE GETX
ROWS 2- VALUE GETY

: GETXY  GETX GETY ;
: SETXY  2DUP [ ' SETXY  DEFER@ COMPILE, ] TO GETY TO GETX ;

: S_EMIT DUP  [ ' EMIT DEFER@ COMPILE, ] 
  DUP	$D = IF  DROP 0		TO GETX BREAK
	$A = IF GETY 1+ ROWS 1- UMIN TO GETY BREAK
  GETY COLS * GETX + 1+
  OVER 8  = IF 2- 0MAX THEN
  MON_SIZE 1- UMIN 
  COLS /MOD TO GETY TO GETX
;

' S_EMIT TO EMIT 

[THEN]

\- GETXY : GETXY  TEXTOUTPUTMODE tm.CursorColumn L@  TEXTOUTPUTMODE tm.CursorRow L@ ;

 REQUIRE CO ForthLib\tools\acc.4th CO
 REQUIRE VIEW ForthLib\tools\view.4th 
 REQUIRE CODE ForthLib\asm\gasm64.4th
\- DROP, : DROP,	$086D8D4800458B48 , ;
\- DUP, : DUP,	$00458948F86D8D48 , ;

FLOAD ForthLib/lib/syscall.4th 

: PAGE   SYSTAB ST*ConOut @  DUP ClearScreen @ 1XSYS DROP ;

FLOAD ForthLib/ansi/key.4th 

[IFNDEF] UZTYPE
: UZTYPE ( uzadr -- )
 SYSTAB ST*ConOut @
 DUP OutputString @ 2XSYS DROP ;
[THEN]

REQUIRE DIR ForthLib\tools\dir.4th 
REQUIRE NC ForthLib\tools\NNC.4th

:NONAME
." WORDS -  List the definition names" CR
." EDIT ( <filename> ) - text editor" CR
." REE  - continue of editing" CR
." E> ( <name> ) - Hyperlink (in EDIT f11 hyperlink, f12 return)" CR
." SEE ( <name> ) - disasm" CR
." DISA ( addr -- ) - disasm" CR
." NC - file manager"  CR
; ->DEFER HELP              

 LASTSTP: : KETST BEGIN KEY DUP EMIT  $20 OR 'q' = UNTIL ; KETST

 LASTSTP: fload work\asmtst.4th 
 LASTSTP: ' GCCOUTPUTRESET DISA
 LASTSTP: SYSTAB ST*ConOut @ ClearScreen perform
 LASTSTP: : KEY?T BEGIN ." <SS>" KEY? UNTIL ; KEY?T
 
LASTSTP: DUP TSTBUF 22 ROT WRITE-FILE H. CLOSE-FILE H.
LASTSTP:  CLOSE-FILE H.
LASTSTP: S" QWERTY" R/W CREATE-FILE H. DUP H.
LASTSTP: e> see 

LASTSTP: DIR ForthSrc 
LASTSTP: COUNT_NAME COUNT 22 dump
LASTSTP: nc

LASTSTP: SEE $C+!
LASTSTP: : $C+! ( c1 a1 -- ) DUP 1+! COUNT + 1- 2DUP H. H. C! ; 

CREATE QWE 'Q' W, 'W' W, 'E' W, 'R' W, 'T' W, 'Y' W, 'Q' W, 0 W,


LASTSTP: COUNT_NAME 22 DUMP

LASTSTP: 'Q' COUNT_NAME $C+! 'E' COUNT_NAME $C+! 'R' COUNT_NAME $C+!


.( TRY) CR
.( SEE ABS) CR
.( ' +  DISA  \ Esc - quit anyother - continue ) CR
.( NC - file manager)  CR
.( E> SEE  \ in EDIT f11 hyperlink, f12 return ) CR
