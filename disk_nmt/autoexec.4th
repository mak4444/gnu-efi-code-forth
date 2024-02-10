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

\- SCFASET : SCFASET ( cfa adr len -- ) SNFAFIND DUP 0= IF -13 THROW THEN NAME>C ! ;
\- CFASET : CFASET  ( cfa <name> -- )  PARSE-NAME SCFASET ;
\- LABSET : LABSET  ( cfa -- )  HERE CFASET ;

\- \EOF : \EOF  ( -- )  BEGIN REFILL 0= UNTIL  POSTPONE \ ;

\- EFI_BLUE   1 CONSTANT EFI_BLUE  
\- EFI_GREEN  2 CONSTANT EFI_GREEN 
\- EFI_RED    4 CONSTANT EFI_RED
\- EFI_BRIGHT 8 CONSTANT EFI_BRIGHT   
: >BG 4 << ; 3 COLOR! 
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
1
[IF]
\- ROW
 25 CONSTANT ROWS
\- COL
 80 CONSTANT COLS

ROWS COLS * CONSTANT MON_SIZE
 
0  VALUE GETX
23 VALUE GETY

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

3 VALUE COLOR@
: S_COLOR!  DUP  [ ' COLOR! DEFER@ COMPILE, ] 
 TO COLOR@ ;

' S_COLOR! TO COLOR!

[THEN]

\- GETXY : GETXY  TEXTOUTPUTMODE tm.CursorColumn L@  TEXTOUTPUTMODE tm.CursorRow L@ ;
\- COLOR@ : COLOR@ TEXTOUTPUTMODE tm.Attribute C@ ;

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

:NONAME
." WORDS -  List the definition names" CR
." EDIT ( <filename> ) - text editor" CR
." REE  - continue of editing" CR
." E> ( <name> ) - Hyperlink (in EDIT f11 hyperlink, f12 return)" CR
." SEE ( <name> ) - disasm" CR
." DISA ( addr -- ) - disasm" CR
; ->DEFER HELP              

 LASTSTP: fload work\asmtst.4th 
 LASTSTP: ' GCCOUTPUTRESET DISA
 LASTSTP: SYSTAB ST*ConOut @ ClearScreen perform
 LASTSTP: : KEY?T BEGIN ." <SS>" KEY? UNTIL ; KEY?T
 
LASTSTP: DUP TSTBUF 22 ROT WRITE-FILE H. CLOSE-FILE H.
LASTSTP:  CLOSE-FILE H.
LASTSTP: S" QWERTY" R/W CREATE-FILE H. DUP H.
LASTSTP: e> see 

LASTSTP: : KETST BEGIN KEY DUP EMIT  $20 OR 'q' = UNTIL ;
LASTSTP: DIR ForthSrc 

.( TRY) CR
.( SEE ABS) CR
.( ' +  DISA  \ Esc - quit anyother - continue ) CR
.( E> SEE  \ in EDIT f11 hyperlink, f12 return ) CR

