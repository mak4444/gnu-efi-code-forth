\ $Id: ansi.f,v 1.3 2008/06/01 18:51:15 ygreks Exp $

\ Originally
\ From: "Ilya S. Potrepalov" <potrepalov@asc-ural.ru>
\ Date: Thu, 23 Feb 2006 13:55:01 +0000 (UTC)
\ Newsgroups: fido7.su.forth
\ Message-ID: <dtkera$r89$93914@www.fido-online.com>

\ ASNI'������� SP-FORTH'�

BASE @ DECIMAL

REQUIRE CASE         lib/ext/case.f
REQUIRE /STRING      lib/include/string.f
REQUIRE [IF]         ~mak\CompIF1.f 
REQUIRE SAVE-INPUT   lib/include/core-ext.f
[DEFINED] WINAPI: [IF]
REQUIRE RENAME-FILE  ~pinka/lib/FileExt.f
[ELSE]
REQUIRE RENAME-FILE  lib/posix/file.f
[THEN]
REQUIRE D0<          lib/include/double.f
REQUIRE ANSI-FILE    lib/include/ansi-file.f

WARNING @  0 WARNING !

DECIMAL

: ?DUP  ?DUP ;  \ ?DUP � SP-FORTH'� state-smart, � ��� �� �� ���������

: CONVERT  ( ud1 c-addr1 -- ud2 c-addr2 )
    \ from gforth
    CHAR+ TRUE >NUMBER DROP
;

VARIABLE SPAN  0 SPAN !
: EXPECT  ( a u -- )
    \ ��� �����, ��� ������
    ACCEPT SPAN !
;

: D.R  ( d +n -- )
    >R DUP >R  DABS  <# #S R> SIGN #>
    R> OVER - 0 MAX SPACES  TYPE
;

\ DPANS 3.4.1.1 Delimiters
\
\ If the delimiter is the _space_ _character_, hex 20 (BL), control
\ characters may be treated as delimiters. The set of conditions,
\ if any, under which a space delimiter matches control characters
\ is implementation defined. 
\
\
\ DPANS 11.3.6 Parsing:
\
\ When parsing from a text file using a space delimiter, 
\ control characters shall be treated the same as the space character.
\
: IS-DELIM  ( c delim -- f )
    DUP BL =
    IF
        SOURCE-ID 0>
        IF
            DROP BL 1+ <
            EXIT
        THEN
    THEN
    =
;

: PARSE  ( "word<c>" c -- c-addr u )
    >R  CharAddr 0
    BEGIN
        GetChar
    WHILE
        >IN 1+!
        R@ IS-DELIM 0=
    WHILE
        1+
    REPEAT
    0
    THEN
    DROP R> DROP
;

: SKIP  ( "<cccc>" c -- )
    BEGIN   GetChar
    WHILE   OVER IS-DELIM
    WHILE   >IN 1+!
    REPEAT  0
    THEN
    2DROP
;

DECIMAL
: WORD  ( c "<cccc>word<c>" -- c-addr )
    DUP SKIP  PARSE 255 MIN
    DUP PAD C!
    DUP 1+ CHARS PAD +  BL SWAP C!
    PAD CHAR+ SWAP CMOVE
    PAD
;
    

\ ANSI said: "If a system provides any standard word for accessing
\ mass storage, it shall also implement the Block word set".
\
\ BLOCK word set
\
\ Table 9.2 - THROW code assignments
\
\ -35     invalid block number

VARIABLE BLK  0 BLK !
: BLOCK   -35 THROW ;
: BUFFER  -35 THROW ;
: FLUSH ;
: LOAD    -35 THROW ;
: SAVE-BUFFERS ;
: UPDATE ;

: BIN ;
: FILE-STATUS  2DROP  0 -21 ; \ unsupported operation


WARNING !
BASE !

\ END OF FILE
