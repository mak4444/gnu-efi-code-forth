\ наблюдение работы методов оптимизазии			Михаил Максимов
REQUIRE [IF] ~MAK\CompIF3.f

\ CREATE    DTST-TAB   0x80 CELLS ALLOT          DTST-TAB   0x80 CELLS ERASE 
\- M@ : M@ @ ;
\- M! : M! ! ;

: DoDTST
\   CR
\ RP@ CELL+ CELL+ @ T_?.NAME>S 
\ RP@ CELL+ @ T_?.NAME>S 
\ R@ T_?.NAME>S 
  :-SET >R BASE M@ >R HEX
 DUP 0<
 IF   ." [0;33m" CR SOURCE TYPE  ." [0;0m" 
 THEN       CR .  ." {" .S ." }"
     BEGIN  ." OP0=" OP0 @ U.
 ." +BP=" OFF-EBP .  ." +AX=" OFF-EAX . ." J=" J_COD U. ." SET=" :-SET DUP U. ." H=" THERE U. \ ." [_0b732]=" $8040b732 @ U.
CR ." OP2=" OP2 @ U. ." OP1=" OP1 @ U. ." OP0=" OP0 @ U.

  OP6 @ UMAX  REST
           KEY
	 DUP $1B = IF ['] DROP TO DTST THEN
		 $20 OR
           DUP 'q' = THROW
           DUP 'd' =
           IF DROP POSTPONE [ S" DDD.F" INCLUDED ] FALSE
           THEN   
           DUP 's' =
           IF DROP  ." <" DUP U. ." >"  FALSE
 \          IF DROP  ." <" .S ." >"  FALSE
           THEN   

           DUP 'n' =
           IF DROP DUP CR ." [0;32m" INST ." [0;0m" CR DROP  FALSE
           THEN   
           DUP 'm' =
           IF DROP DUP CR ." [0;32m" INST CR INST  ." [0;0m" CR DROP  FALSE
           THEN   
     UNTIL
 R> BASE M!  R> TO :-SET ;


\ [ ' DoDTST TO DTST ]
