
1 VALUE R/O ( -- fam ) \ 94 FILE
\ fam - 
\ 

2 VALUE W/O ( -- fam ) \ 94 FILE
\ fam - 
\ 

3 VALUE R/W ( -- fam ) \ 94 FILE

: CFNAME-SET  ( adr len -- )
 DUP 1+ >R  CURFILENAME CURFILENAME R@ + CFNAME_SIZE R> - CMOVE>
CURFILENAME $! ;

: CFNAME-FREE  ( -- )
 CURFILENAME COUNT + CURFILENAME
 CFNAME_SIZE CURFILENAME C@ - 255 +  CMOVE ;

\ : VVV #H. >PF >DPF ;

: INCLUDE-FILE ( i*x fileid -- j*x ) \ 94 FILE
\	." INC"
	>IN  @ >R
	CURSTR @ >R CURSTR 0!
	SOURCE-ID >R  TO SOURCE-ID
\	CR RP@ 6 >> $F AND SPACES ." save=<" SOURCE TYPE ." >"
	SOURCE
	RP@ OVER ALIGNED - RP!
	RP@ SWAP CMOVE
	SOURCE 2>R 
	['] FQUIT CATCH	SAVEERR	
	DUP  IF cr ." in <" CURFILENAME COUNT TYPE ." >:" CURSTR @ . THEN \	['] NOOP CATCH	SAVEERR	
	2R> SOURCE!
	RP@ SOURCE CMOVE
\	CR ." IF=<" SOURCE TYPE ." >"
	RP@ SOURCE NIP ALIGNED + RP!
\	CR RP@ 6 >> $F AND SPACES ." rest=<" SOURCE TYPE ." >"
	R> TO SOURCE-ID
	R> CURSTR !
	R> >IN ! THROW  
;

T: VIEW_{
?YDP><DP
   HERE VIEW_LINK , TO VIEW_LINK  0 ,
?YDP><DP
;

T: }VIEW
?YDP><DP
   HERE  VIEW_LINK , TO VIEW_LINK -1 ,
   CURFILENAME COUNT S",
?YDP><DP

;

T: INCLUDED  ( c-addr u ---- )
\ Open the file with name c-addr u and interpret all lines contained in it.
\	CR RP@ 6 >> $F AND SPACES ." TO   <" 2DUP TYPE ." >"
	2DUP	R/O  OPEN-FILE THROW >R
	CFNAME-SET
        VIEW_{
	R@ ['] INCLUDE-FILE
	CATCH
	}VIEW
\	CR RP@ 8 + 6 >> $F AND SPACES  ." FROM <" CURFILENAME COUNT TYPE ." >"  DUP . 
	CFNAME-FREE
	R> CLOSE-FILE DROP
	THROW
;


: FLOAD  PARSE-NAME INCLUDED ;

