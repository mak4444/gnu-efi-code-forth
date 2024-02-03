( CASE-INS - case sensitivity switcher )

REQUIRE CEQUAL-U lib/ext/caseins-tools.f 

VARIABLE CASE-INS \ switcher
CASE-INS ON

: USEARCH-WORDLIST ( c-addr u wid --- 0 | xt 1 xt -1)
  CASE-INS @ 0= IF  [ ' SEARCH-WORDLIST DEFER@ COMPILE, ] BREAK
	@
	BEGIN   DUP
	WHILE
	>R 2DUP
		R@
	 COUNT CEQUAL-U
		IF	2DROP
			R@ NAME> 
			R> NAME>F  @ 1 AND 1- 1 OR
			 EXIT
		THEN 
 	R> CDR
	REPEAT
	2DROP DROP 0 \ Not found.
;

' USEARCH-WORDLIST TO SEARCH-WORDLIST

