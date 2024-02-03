
REQUIRE NUMBER? ~mak/lib/fpcnum.f

TRUE VALUE QUICK
: SL\ POSTPONE \ ; IMMEDIATE

REQUIRE TC_?LIMIT ~mak/lib/THERE/STAT.F

~mak/lib/THERE/mthere.f
~mak/lib/THERE/texec.4 
~mak/lib/THERE/RECOM.F
: _, DROP 0 C, ;


