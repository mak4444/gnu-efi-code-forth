( RND DAY 04.05.2001 )

WINAPI: GetTickCount KERNEL32.DLL

VARIABLE RND

: SEED ( U -- )
    RND !
;

: RANDOM ( -- U )
   RND @ 0x8088405 * 1+
   DUP RND !
;

: CHOOSE ( U1 -- U2 )
\ U2 - RANDOM NUMBER FROM 0 TO U1
   RANDOM UM* NIP
;

: RANDOMIZE GetTickCount SEED ;

RANDOMIZE