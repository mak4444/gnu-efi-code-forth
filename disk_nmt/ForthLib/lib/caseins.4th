( CASE-INS - case sensitivity switcher )

REQUIRE CHAR-UPPERCASE ForthLib\spf\uppercase.4th

VARIABLE CASE-INS \ switcher
CASE-INS ON

: USEARCH-NFA ( c-addr u wid --- 0 | nfa )
  CASE-INS @ 0= IF  [ ' SEARCH-NFA DEFER@ COMPILE, ] BREAK
	@
	BEGIN   DUP \  CR ." S=" DUP H.  DUP 8 TYPE
	WHILE
	>R 2DUP	R@ COUNT CEQUAL-U
		IF 2DROP R>
		BREAK
 	R> CDR
	REPEAT
	2DROP DROP 0 \ Not found.
;

' USEARCH-NFA TO SEARCH-NFA


