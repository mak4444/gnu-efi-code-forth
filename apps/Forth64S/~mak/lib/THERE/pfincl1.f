
: WPF>  ( -- w )
       PF>
       PF>    8 LSHIFT OR ;

: DPF>  ( -- c )
    WPF>
    PF> 0x10 LSHIFT OR
    PF> 0x18 LSHIFT OR ;

: SPF> ( addr -- addr+1 len)
 	DUP>R
	PF> DUP 0= IF DROP RDROP 0 EXIT THEN
	BEGIN PF> R@ C! R> 1+ >R 1- DUP 0= UNTIL 
	DROP R> OVER - ;

: >WPF  ( w -- )
	DUP >PF	8 RSHIFT >PF ;

: >DPF  ( n -- )
	DUP >PF	8 RSHIFT
	DUP >PF	8 RSHIFT >WPF ;

: >SPF ( addr len  -- addr )
	DUP 0= IF 2DROP EXIT THEN
\+ OUTPUT	BOUNDS	BEGIN COUNT >PF 2DUP U> 0= UNTIL	2DROP
\- OUTPUT	TO_COM_
  ;


: >$PF ( addr len  -- )
  DUP >PF >SPF ;

: PFS> BEGIN PF> KEY? UNTIL ;
