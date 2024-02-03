\ Mihail Maksimov, forth@inbox.ru mak@rtc.ru

REQUIRE $! ~mak\place.f

[UNDEFINED] DBG_DO
[IF]   VECT DBG_DO  ' 2DROP TO DBG_DO
[THEN]

' DBG_DO VALUE 'DBG_DO

[UNDEFINED] S",
[IF] 
: S", ( addr u -- ) \ компиляция строки, заданной addr u, в виде строки со счетчиком
  DUP C, DP @ SWAP DUP ALLOT CMOVE 0 C, ;
[THEN]

[UNDEFINED]  GetCurrentDirectoryA
[IF] WINAPI: GetCurrentDirectoryA   KERNEL32.DLL
[THEN]

[DEFINED]  (INCLUDED)
[IF]
VARIABLE DBG_DP
 0x6000 ALLOCATE THROW DBG_DP !
\ HERE DBG_DP ! 0x6000 ALLOT

: DBG><DP DP @ DBG_DP @
       DP ! DBG_DP ! ;
[ELSE] : DBG><DP ; IMMEDIATE
[THEN]

\ MODULE: DBG_MOD

VECT OLD_<PRE>
VECT OLD_(INCLUDED)

[UNDEFINED] DBG_(INCLUDED)
[IF]

\ CR .( DBG_(INCLUDED )

180 CONSTANT DIR_SIZE

CREATE  DIR_STOR  DIR_SIZE ALLOT
CREATE  DIRF_STOR  DIR_SIZE 10 + ALLOT

0 VALUE DBG_CURFILE

: DBG_CURFILE,
  DBG><DP
  HERE TO DBG_CURFILE
  CURFILE @ 1+ C@ [CHAR] : =
  IF  CURFILE @ ASCIIZ> S",
  ELSE    DIR_STOR DIR_SIZE GetCurrentDirectoryA DROP
          DIR_STOR ASCIIZ> DIRF_STOR $! 
                     S" \" DIRF_STOR $+!
        CURFILE @ ASCIIZ>  DIRF_STOR $+!
        DIRF_STOR COUNT  S",
  THEN
  DBG><DP ;

: DBG_(INCLUDED)
  DBG_CURFILE >R
  DBG_CURFILE,
  OLD_(INCLUDED)
  R> TO DBG_CURFILE
;

[THEN]

: DBG_<PRE>
  STATE @
  IF  CURSTR @ 1- 0 MAX LIT,
      DBG_CURFILE LIT,
      'DBG_DO COMPILE,  OLD_<PRE>
  THEN
;

\ EXPORT

VARIABLE DBG_COUNT  DBG_COUNT 0!

: ?NO_DBG  DBG_COUNT @ ABORT" DBG" ; IMMEDIATE

[DEFINED]  (INCLUDED)
[IF]

: DBG_START
  DBG_CURFILE,
  [']  (INCLUDED) >BODY @ TO OLD_(INCLUDED)
  [']  DBG_(INCLUDED) TO (INCLUDED)

  ['] <PRE> >BODY @ TO OLD_<PRE>
  [']  DBG_<PRE> TO <PRE> 
  1 DBG_COUNT !
;

: DBG_STOP
  ['] OLD_(INCLUDED) >BODY @ TO (INCLUDED)
  ['] OLD_<PRE> >BODY @ TO <PRE>
  DBG_COUNT 0!
;
: <DBG>
  DBG_COUNT @ 0=
  IF DBG_START EXIT THEN
  DBG_COUNT  1+!
;         IMMEDIATE

: <\DBG>
  -1 DBG_COUNT  +!
  DBG_COUNT @ 0=
  IF DBG_STOP THEN
;         IMMEDIATE
[THEN]

\ ;MODULE

