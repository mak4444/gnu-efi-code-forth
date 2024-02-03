.( autoexec.4th) CR

\- REQUIRED : REQUIRED ( waddr wu laddr lu -- )
\- REQUIRED  2SWAP  SFIND  IF DROP 2DROP EXIT  ELSE 2DROP  INCLUDED THEN ;
\- REQUIRE : REQUIRE ( "word" "libpath" -- ) PARSE-NAME PARSE-NAME  REQUIRED ;
\- LAST-NON 0 VALUE LAST-NON
\- :NONAME : :NONAME ( C: -- colon-sys ) ( S: -- xt ) \ 94 CORE EXT
\- :NONAME  HERE DUP TO LAST-NON [COMPILE] ] ;
\- RECURSE : RECURSE \ Compile a call to the current (not yet finished) definition.
\- RECURSE	LAST @ NAME> LAST-NON UMAX COMPILE, ; IMMEDIATE
\- \EOF : \EOF  ( -- )  BEGIN REFILL 0= UNTIL  POSTPONE \ ;

\- EFI_BLUE   1 CONSTANT EFI_BLUE  
\- EFI_GREEN  2 CONSTANT EFI_GREEN 
\- EFI_RED    4 CONSTANT EFI_RED
\- EFI_BRIGHT 8 CONSTANT EFI_BRIGHT   
: >BG 4 << ;
 3 COLOR! 
 ' KEY2 TO KEY

 REQUIRE SEE ForthLib\asm\disgasm.4th 
 FLOAD ForthLib\tools\words.4th 
 REQUIRE CASE-INS ForthLib\lib\caseins.4th 
 REQUIRE STRUCTURES{ ForthLib\lib\structures.4th 

 FLOAD ForthLib\include\eficon.4th 
\- GETXY : GETXY  TEXT_OUTPUT_MODE tm.CursorColumn L@  TEXT_OUTPUT_MODE tm.CursorRow L@ ;
\- COLOR@ : COLOR@ TEXT_OUTPUT_MODE tm.Attribute C@ ;

 REQUIRE CO ForthLib\tools\acc.4th CO
 REQUIRE VIEWS_SEARCH ForthLib\tools\defview.4th 
 REQUIRE VIEW ForthLib\tools\view.4th 

.( TRY) CR
.( SEE ABS) CR
.( ' +  DISA  \ q - quit anyother - continue ) CR
.( E> SEE  \ in EDIT f11 hyperlink, f12 return ) CR


