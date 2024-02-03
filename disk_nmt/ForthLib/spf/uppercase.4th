
: CHAR-UPPERCASE ( c -- c1 )
  DUP [CHAR] a [CHAR] z 1+ WITHIN IF 32 - EXIT THEN
;

: UPPERCASE ( addr1 u1 -- )
  OVER + SWAP ?DO
    I C@ CHAR-UPPERCASE I C!
  LOOP ;
