
\- UCOMPARE : UCOMPARE COMPARE ;
22 CELLS CONSTANT VIEW_FSTACK_SIZE
CREATE VIEW_FSTACK 0 , 0 , VIEW_FSTACK_SIZE ALLOT

: SNFAFIND ( addr len --- 0| nfa )
  CONTEXT
  BEGIN	DUP @ \ DUP H.
  WHILE	>R
	2DUP  R@  @  SEARCH-NFA ?DUP
	IF    RDROP NIP NIP EXIT \ Exit if found.
	THEN
	R> CELL+
  REPEAT @ NIP NIP
;

: VIEWS_SEARCH  ( adr len -- nfa path | 0  )
  SNFAFIND
 DUP 0= IF BREAK
 DUP  VIEW_LINK
 BEGIN 2DUP U> WHILE @ REPEAT \ for 
 BEGIN  DUP
 WHILE
	2DUP U> IF 2DROP  VIEW_FSTACK @ CELL+ CELL+ BREAK
	DUP CELL+ @ 
	IF   VIEW_FSTACK VIEW_FSTACK CELL+ VIEW_FSTACK_SIZE MOVE
	      DUP VIEW_FSTACK !
	ELSE VIEW_FSTACK CELL+  VIEW_FSTACK VIEW_FSTACK_SIZE MOVE
	THEN
	@
 REPEAT NIP NIP
;
