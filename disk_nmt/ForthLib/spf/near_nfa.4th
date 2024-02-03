: WL_NEAR_NFA_N ( addr nfa - addr nfa | addr 0 )
\ cr ." ~~~" over h. cr
   BEGIN 2DUP DUP IF NAME> THEN U<
   WHILE \ ." <" dup NAME> h. dup count type bl emit ." >" CR
  CDR
   REPEAT
;

: N_UMAX ( nfa nfa1 -- nfa|nfa1 )
 OVER DUP IF NAME> THEN
 OVER DUP IF NAME> THEN U< IF NIP EXIT THEN DROP ;

: WL_NEAR_NFA_M (  addr wid - nfa2 addr | 0 addr )
   0 -ROT
   @  
   BEGIN  DUP
   WHILE  WL_NEAR_NFA_N  \  nfa addr nfa1
       SWAP >R 
       DUP  >R  N_UMAX 
       R>  DUP  IF CDR THEN 
       R>  SWAP

   REPEAT DROP
;

: NEAR_NFA ( addr - nfa addr | 0 addr )
   0 SWAP 
   VOC-LIST
   BEGIN  @ DUP
   WHILE DUP  >R
\- INLINE?  CELL-
  CELL-
  WL_NEAR_NFA_M
   >R  N_UMAX  R>  R>
   REPEAT DROP
;
