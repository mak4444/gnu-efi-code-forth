\ Get current local date and time
\
\ TIME&DATE ( -- sec min hour day month year )
\
\ ms@ ( -- n ) \ the number of milliseconds since some fixed point in the past

REQUIRE XAPI_1: lib\WAPI.4

REQUIRE [IF] ~mak/CompIF1.f

\ _SYSTEMTIME
0
2 FIELD wYear
2 FIELD wMonth
2 FIELD wDayOfWeek
2 FIELD wDay
2 FIELD wHour
2 FIELD wMinute
2 FIELD wSecond
2 FIELD wMilliseconds
CONSTANT /SYSTEMTIME
CREATE SYSTEMTIME /SYSTEMTIME ALLOT

KERNEL32DLL XAPI_1: GetLocalTime GetLocalTime
KERNEL32DLL XAPI_0: GetTickCount GetTickCount

: TIME&DATE ( -- sec min hr day mt year ) \ 94 FACIL
  SYSTEMTIME GetLocalTime DROP
  SYSTEMTIME wSecond W@
  SYSTEMTIME wMinute W@
  SYSTEMTIME wHour W@
  SYSTEMTIME wDay W@
  SYSTEMTIME wMonth W@
  SYSTEMTIME wYear W@
;

: ms@ GetTickCount ;


: _TIME_ ( Sec Min Hour -- addr u )
    <#
  ROT	0 # #  2DROP  [CHAR] : HOLD
 SWAP	0 # #  2DROP  [CHAR] : HOLD
	0 # #  2DROP 0 0 #>
;



