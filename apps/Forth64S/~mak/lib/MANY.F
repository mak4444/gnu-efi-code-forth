
: $=  ( a1 u1 a2 u2 -- flag )  COMPARE 0= ;
: <NEXT>  BL WORD COUNT ;
: PREPARSE >IN @ >R <NEXT> R> >IN ! ;
: MANY:  ( ... -- ... )   \ Usage: many <word> ... ;
   ' >R
   BEGIN
     PREPARSE 2DUP S" ;" COMPARE
   WHILE
     2DUP S" (" $= IF 2DROP POSTPONE (  ELSE
     2DUP S" \" $= IF 2DROP POSTPONE \  ELSE
               NIP IF R@ EXECUTE        ELSE
                      REFILL 0= ABORT" ; missing after many:"
     THEN THEN THEN
   REPEAT 2DROP
   BL WORD DROP  \ Skip ;
   R> DROP ;

