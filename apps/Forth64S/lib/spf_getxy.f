REQUIRE API_2: lib\WAPI.4

KERNEL32DLL XAPI_2: GetConsoleScreenBufferInfo GetConsoleScreenBufferInfo
KERNEL32DLL XAPI_2: SetConsoleCursorPosition SetConsoleCursorPosition

: COORD ( x y -- COORD )
  16 LSHIFT OR 
;

: AT-XY ( x y -- )
\ 
  COORD H-STDOUT
  SetConsoleCursorPosition DROP \ ERR THROW
;

: SETXY AT-XY ;

CREATE CONSOLE_SCREEN_BUFFER_INFO 22 ALLOT
: SBI CONSOLE_SCREEN_BUFFER_INFO 20 DUMP ;

: AT-XY? ( -- x y )
\ 
  CONSOLE_SCREEN_BUFFER_INFO H-STDOUT
  GetConsoleScreenBufferInfo  DROP
  CONSOLE_SCREEN_BUFFER_INFO 4 + DUP W@ SWAP 2 + W@ ;

: GETXY AT-XY? ;
