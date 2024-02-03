REQUIRE AT-XY ~day\common\console.f

: SETXY  ( x y -- )  AT-XY ;

CREATE CONSOLE_SCREEN_BUFFER_INFO 22 ALLOT

WINAPI: GetConsoleScreenBufferInfo kernel32.dll
\ WINAPI: SetConsoleTextAttribute		KERNEL32.DLL

: GETXY  ( -- x y )
  CONSOLE_SCREEN_BUFFER_INFO H-STDOUT GetConsoleScreenBufferInfo DROP
  CONSOLE_SCREEN_BUFFER_INFO 4 + DUP W@ SWAP 2+ W@ ;

: GETMAXXY  ( -- x y )
  CONSOLE_SCREEN_BUFFER_INFO H-STDOUT GetConsoleScreenBufferInfo DROP
  CONSOLE_SCREEN_BUFFER_INFO 4 + 4 +  2+ 8 + DUP W@ SWAP 2+ W@ ;

: COLS GETMAXXY DROP ; 
: ROWS GETMAXXY NIP ; 
