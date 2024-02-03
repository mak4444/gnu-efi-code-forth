REQUIRE CHAR-UPPERCASE ~ac/lib/string/uppercase.f
\ REQUIRE /TEST ~profit/lib/testing.f

\ TRUE - strings are equal ignoring case
: CEQUAL-U ( a1 u1 a2 u2 -- flag )
  ROT TUCK <> IF DROP 2DROP FALSE EXIT THEN
  0 ?DO ( a1i a2i ) 2DUP
  C@ CHAR-UPPERCASE SWAP C@ CHAR-UPPERCASE <> IF 2DROP UNLOOP FALSE EXIT THEN
  SWAP CHAR+ SWAP CHAR+
  LOOP 2DROP TRUE
;

