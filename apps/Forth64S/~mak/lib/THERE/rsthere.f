REQUIRE T_@ ~mak/lib/THERE/there.f

[IFNDEF] ANDC : ANDC INVERT AND ;
[THEN]

VECT T_C@>  ( -- c )
' COM-GET TO T_C@>

0xF00  VALUE #WAIT
VARIABLE WAIT_COUNT

: T_C@?  ( -- n )
       COM-CNT ;

VARIABLE KEY?_BUFF
VARIABLE KEY_BUFF

0xF00  VALUE #WAIT
VARIABLE WAIT_COUNT

VECT KEY_ABORT

: KEY_ABORT_ ( c -- )
 [CHAR] Q = IF
 CR ." QQ44"
 -28 THROW THEN ;

' KEY_ABORT_ TO KEY_ABORT


1 3 LSHIFT CONSTANT KBDBUF-SIZE
KBDBUF-SIZE 1- CONSTANT KBDBUF-MASKA


CREATE KBDBUF    KBDBUF-SIZE ALLOT
CREATE KBDBUFIN  1 ,
CREATE KBDBUFOUT 0 ,

: KBD-CNT       ( -- count )  \  get how many bytes are in the buffer
    KBDBUFIN @  KBDBUFOUT @ -  1-  KBDBUF-MASKA AND  ;

: KBD-PUT       ( Wchar -- )   \  get a character from the buffer
     KBDBUFIN  @  KBDBUF-MASKA  AND
     KBDBUF  +   C!     \ put the byte
     KBDBUFIN   1+!         \ increment the pointer
;

: KBD-GET  ( -- Wchar )   \  get a character from the buffer
      KBDBUFOUT 1+!             \ and increment the pointer
      KBDBUFOUT @ KBDBUF-MASKA AND
      KBDBUF +   C@           \ get the byte
;

: T-C@?
\	KEY? IF KEY DROP THEN
	#WAIT WAIT_COUNT !
	BEGIN \ ." <" FJB
 T_C@? \ ." >" FJB
 0=
	WHILE	WAIT_COUNT @ 0= 0 AND ABORT" port is't answered" \ !!!!!!
		-1  WAIT_COUNT +!
		KEY? IF ." Ka"  EKEY? H.  KEY DUP KBD-PUT  EKEY? H.
\			." key=" dup h. cr
			0x11 = IF
            KEY? IF KEY DROP THEN
 CR ." QQ55" cr
	-28 THROW THEN THEN
		WAIT_COUNT @ 0x1FF AND 0=
		   IF
		   FJB KEY? IF ." Kb" KEY  DUP KBD-PUT
			 KEY_ABORT
		   -1 KEY?_BUFF !
		   THEN  1 SLEEP
		THEN
	REPEAT ;


: T-C@  ( -- c )
  T-C@?
  T_C@>
 ;

: T-W@  ( -- w )
       T-C@
       T-C@    8 LSHIFT OR ;

: T-L@  ( -- c )
    T-W@
    T-C@ 0x10 LSHIFT OR
    T-C@ 0x18 LSHIFT OR
;

: T-@  ( -- c )
    T-L@

    T-C@ 0x20 LSHIFT OR
    T-C@ 0x28 LSHIFT OR
    T-C@ 0x30 LSHIFT OR
    T-C@ 0x38 LSHIFT OR


 ;

: T_C@?  ( -- n )
       COM-CNT ;

VECT T_C@>  ( -- c )
' COM-GET TO T_C@>

:  COM? \ F7_ED
\  COM-LAST 0x7 AND 0= IF CR THEN
  ." Com=" COM-LAST DUP  BL < IF ." *"  ELSE DUP EMIT THEN  ."  " H. FJB
 ;

CREATE EXE_TAB
0x100 
 ' COM? ,  1- DUP [IF]  >IN 0! [THEN]
DROP


: PWAIT ( cfa -- )
\ CR ." PWAIT=" DUP >NAME ID.
    >R \ F7_ED
    BEGIN
	BEGIN	T-C@ DUP 0xFF <>
	WHILE	 DUP EMIT $D = IF $A EMIT THEN
	REPEAT
	DROP T-C@
\ CR ." PW=" DUP H.  FJB

CELLS EXE_TAB  +
 @ DUP
  R@ \ 2DUP H. H.
 <> 
    WHILE  EXECUTE 
    REPEAT
    EXECUTE RDROP ;

: MAIN_STEP_
 T-C@  DUP #MAX U<
 IF  #MAX UMIN CELLS  EXE_TAB  + @ EXECUTE
 ELSE ." _" EMIT FJB
 THEN
 ;

: RS_MAIN_S
  BEGIN  COM-CNT \ KEY? 0= AND
  WHILE \ T-C@   ." RS="  H. \ 
	T-C@ DUP $FF =
	IF DROP MAIN_STEP_ 
	ELSE DUP EMIT $D = IF $A EMIT THEN
	THEN
  REPEAT ;

: RST_C@ (  addr -- c )    
	$FF UEMIT
    #C@ UEMIT  8UEMIT 
    ['] T-C@ PWAIT ;


: PPPWAIT ( cfa -- )
\ CR ." PWAIT=" DUP >NAME ID.
    >R \ F7_ED
." %"
    BEGIN
	BEGIN	T-C@ DUP 0xFF <>
	WHILE ." ^" DUP EMIT $D = IF $A EMIT THEN
	REPEAT

	DROP T-C@
\ CR ." PW=" DUP H.  FJB
." &"
CELLS EXE_TAB  +
 @ DUP
  R@ \ 2DUP H. H.
 <> 
    WHILE  EXECUTE 
    REPEAT
    EXECUTE RDROP ;

: RRRST_C@ (  addr -- c )    
	$FF UEMIT
    #C@ UEMIT  8UEMIT 
    ['] T-C@ PPPWAIT ;

' RRRST_C@  TTT_C@_SET

: P_C! ;

: RST_C! ( c addr -- ) 
\	2DUP	&T_C!
	$FF UEMIT
    #C! UEMIT SWAP UEMIT   8UEMIT 
\    ['] P_C! PWAIT

 ;

: P_W! ;

: RST_W! ( c addr -- )
\	2DUP	&T_W!
	$FF UEMIT
    #W! UEMIT SWAP  2UEMIT  8UEMIT 
\    ['] P_W! PWAIT
 ;


: RST_W@ (  addr -- W ) 
	$FF UEMIT
    #W@ UEMIT  8UEMIT 
    ['] T-W@ PWAIT ;

: P_L! ;

: RST_L! ( c addr -- ) 
\	2DUP	&T_L!
	$FF UEMIT
    #L! UEMIT SWAP  4UEMIT   8UEMIT 
\    ['] P_! PWAIT
 ;


: RST_L@ (  addr -- W )     
	$FF UEMIT
    #L@ UEMIT  8UEMIT
    ['] T-L@ PWAIT
 ;

: P_! ;

: RST_! ( c addr -- ) 
\	2DUP	&T_!
	$FF UEMIT
    #! UEMIT SWAP  8UEMIT   8UEMIT 
\    ['] P_! PWAIT
 ;

: RST_@ (  addr -- W )     
	$FF UEMIT
    #@ UEMIT  8UEMIT
    ['] T-@ PWAIT
 ;

: S_INPUT
 ?DUP
    IF T-@ >R 1- RECURSE R>
    THEN ;

: PEXECUTE  T-@ 
\  ." PEXECUTE=" DUP H. CR
 DUP 0< IF -4 THROW THEN
 S_INPUT
 ;

: S_OUT
  DEPTH 
    IF >R RECURSE R> 8UEMIT
    THEN ;

: RSTEXECUTE
\   F7_ED
	$FF UEMIT
     #EXEC UEMIT
    DEPTH  8UEMIT
    S_OUT
    ['] PEXECUTE PWAIT
 ;

: RSTEXECUTE-
\   F7_ED
	$FF UEMIT
     #EXEC UEMIT
    DEPTH  8UEMIT
    S_OUT
;

: P_H.
\ F7_ED
  T-@ H.
 FJB ;

' P_H.  EXE_TAB #H. CELLS + !

: P_E    T-C@  EMIT FJB ;

' P_E  EXE_TAB #E CELLS + !

\ EOF

0 VALUE 1D?

1 [IF]
: P_KEY
  KBD-CNT
  IF KBD-GET \  ." K^="
  ELSE  KEY \  ." K~="
  THEN  \  DUP H.
   UEMIT
;

: P_KEY?
  KEY? KBD-CNT OR
 UEMIT
 1 KBDBUFIN ! KBDBUFOUT 0!
\  BEGIN KEY? WHILE KEY DROP REPEAT
;

[ELSE]

: P_KEY
  KEY  UEMIT

;


: P_KEY?
  KEY? UEMIT
;

[THEN]

' P_KEY  EXE_TAB #KEY CELLS + !

' P_KEY?  EXE_TAB #KEY? CELLS + !

' CR  EXE_TAB #CR CELLS + !

' T-C@  EXE_TAB #C@ CELLS + !
' T-W@  EXE_TAB #W@ CELLS + !
' T-L@  EXE_TAB #L@ CELLS + !
' T-@   EXE_TAB #@  CELLS + !

' PEXECUTE  EXE_TAB #EXEC CELLS + !

0 VALUE V_START

: P_START  CR ." START" FJB TRUE TO V_START ;

' P_START  EXE_TAB #START CELLS + !

: P_ERROR  ." FROM IPC " FJB ER-U 0! T-@ THROW ;

' P_ERROR  EXE_TAB #ERROR CELLS + !

: P_KS  TRUE ABORT" KS ERROR" ;

' P_KS  EXE_TAB #KS CELLS + !

' P_KS  EXE_TAB #KS CELLS + !

' P_C!  EXE_TAB #C! CELLS + !
' P_W!  EXE_TAB #W! CELLS + !
' P_L!  EXE_TAB #L! CELLS + !
' P_!   EXE_TAB #!  CELLS + !

: P_HERE  HERE UEMIT ;

' P_HERE	EXE_TAB #HERE CELLS + !
' NOOP		EXE_TAB #WDOG CELLS + !



: RS_MODE
['] RST_C!  TO T_C!
['] RST_W!  TO T_W!
['] RST_C@  TO T_C@
['] RST_W@  TO T_W@
['] RST_L!  TO T_L!
['] RST_L@  TO T_L@
['] RST_!   TO T_!
['] RST_@   TO T_@
\ ['] RST_2!  TO T_2!
\ ['] RST_2@  TO T_2@
['] RSTEXECUTE TO T_EXECUTE
 ['] RS_MAIN_S TO MAIN_S
 ;



0x210 CONSTANT W@SIZE

CREATE W@ADDR  W@SIZE 1+ CELLS ALLOT
CREATE W@DATA  W@SIZE 1+ 2*    ALLOT

: W@INIT
\ ." W@INIT "
W@ADDR W@SIZE 1+ CELLS 0xFF FILL ;

W@INIT

: W@ADDR!  W@ADDR W@ADDR CELL+	W@SIZE CELLS	CMOVE>   W@ADDR  ! ;
: W@DATA!  W@DATA W@DATA 2+	W@SIZE 2*	CMOVE>   W@DATA W! ;
0 VALUE LASTW@ADDR
: ?W@
  W@ADDR
  BEGIN 2DUP @ = IF NIP W@ADDR - 2/ W@DATA + DUP TO LASTW@ADDR W@ TRUE BREAK
        CELL+ DUP  W@ADDR W@SIZE CELLS + =
  UNTIL DROP FALSE ;


: QRST_W@
 ?W@ IF BREAK
\ ." >W@=" DUP H.
  DUP W@ADDR! 
 RST_W@
  DUP W@DATA! 
 ;

: QRST_C@
 DUP 1 AND
 IF	1-
	?W@ IF 8 RSHIFT BREAK
	1+
 ELSE
	?W@ IF 0xFF AND BREAK
 THEN
\ ." >C@=" DUP H.
 RST_C@
 ;

: QRST_W!
  DUP W@ADDR! 
 OVER W@DATA! 
  RST_W! ;

: QRST_!
   DUP  W@ADDR!  DUP  2+ W@ADDR!
  OVER  W@DATA! OVER 0x10 RSHIFT  W@DATA!

	RST_!
\ W@INIT
 ;


: QRSTEXECUTE  RSTEXECUTE  W@INIT ;
: QRST_C!

	DUP 1 ANDC ?W@ IF DROP 2DUP  1 AND LASTW@ADDR +  C! DUP THEN
	DROP
	RST_C!
\	W@INIT
 ;

: QRST_@
	DUP>R	QRST_W@
	R> 2+	QRST_W@ 0x10 LSHIFT OR ;


: QRS_MODE
['] QRST_C!  TO T_C!
['] QRST_W!  TO T_W!
['] QRST_C@  TO T_C@
['] QRST_W@  TO T_W@
['] QRST_!   TO T_!
['] QRST_@   TO T_@
\ ['] RST_2!  TO T_2!
\ ['] RST_2@  TO T_2@
['] RSTEXECUTE TO T_EXECUTE
 ['] RS_MAIN_S TO MAIN_S
 ;

 QRS_MODE

