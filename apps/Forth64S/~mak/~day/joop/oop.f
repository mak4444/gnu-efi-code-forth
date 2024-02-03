\ 09.Mar.2002 Sat 20:25  ruv
\ ��������� �� ������� ONLY FORTH DEFINITIONS
\ � ����� � ��������� �������  OO_Support

( Yet another oop extention for sp-forth - just oop :)
( Dmitry Yakimov 2000 [c] )

: SWAP-CURRENT \ PUSH-CURRENT ( wid1 -- wid2 ) \ SWAP-CURRENT
  GET-CURRENT SWAP SET-CURRENT
;

  

REQUIRE MODULE: lib/ext/spf_modules.f
REQUIRE [IF] ~mak/CompIF1.f
REQUIRE [IFDEF] ~nn\lib\ifdef.f


HERE

MODULE: OO_Support

\ structure of class:
0
CELL -- .myself     \ link on myself
CELL -- .methods    \ wid of methods
CELL -- .size       \ size of class instance
CELL -- .parent     \ parent class
CELL -- .name       \ link to class name
CELL -- .variables  \ wid of variables
CONSTANT /class

\ ��� ���������� ������ ORDER: 
\ CONTEXT: wid_of_vars FORTH
\ CURRENT: wid_of_methods

USER ERR-M

EXPORT 

[IFDEF] USER-VALUE
USER-VALUE self
[ELSE]
  0 VALUE self
[THEN]

: this self ;

: WITH  ( oid -- )
  TO self
;

: UnknownMsg ( -- a u )
  ERR-M @ COUNT
;

DEFINITIONS

VOCABULARY ClassContext

DEFINITIONS

: class ( oid - u) @ ;
: len ( cid - u) .size @ ;


0 [IF]

: execMessage ( addr u wid -- ... )
  SEARCH-WORDLIST
  IF   EXECUTE
  ELSE S" :unknown" self @ .methods @ RECURSE
  THEN
;
: sendMessage ( ... addr u oid -- ... )
  self >R
  DUP TO self
  @ .methods @  
  execMessage
  R> TO self
;

: sendMessage ( ... addr u oid -- ... )
  self >R
  DUP TO self
  Linked        IF
  EXECUTE       ELSE
  $Unknown COUNT self @
  RECURSE       THEN
  R> TO self
;
[ELSE]
\ ---
\ =========================
\ ����������!  ������:
\  ( "���"/�������������  �����/������  --  xt/������������ true | false )

CREATE $Unknown  S" :unknown" S", 0 C,

EXPORT

: RESOLVE-LINK ( addr u oid -- xt true | false )
  @ .methods @
  SEARCH-WORDLIST
;
: ResolveLink ( addr u oid -- xt )
  DUP >R
  RESOLVE-LINK IF RDROP EXIT THEN
  $Unknown COUNT R> RESOLVE-LINK 0= ABORT" Object hasn't method ':unknown'."
;
: ExecuteMethod ( i*x xt oid -- j*x )
   self >R
   TO self
   EXECUTE
   R> TO self
;
\ DEFINITIONS
\ =========================

: sendMessage ( ... addr u oid -- ... )
  self >R
  DUP TO self
  ResolveLink CATCH
  R> TO self  THROW
;
[THEN]


DEFINITIONS

0 VALUE message_does


: message, ( oid )
  CREATE LATEST ,
  [ HERE TO message_does ]
  DOES> @ DUP ERR-M !
        COUNT ROT sendMessage
;

EXPORT

: message: ( oid )
  >IN @ NextWord SFIND
  IF 2DROP
  ELSE 2DROP >IN ! 
       message,
  THEN
;

DEFINITIONS

: sendVariable ( ... addr u oid -- ... )
    DUP >R @ .variables @ 
    SEARCH-WORDLIST
    IF
      >BODY @ R> +
    ELSE
      R@ TO self
      S" :unknown" R> @ .methods @ SEARCH-WORDLIST DROP EXECUTE
    THEN
;

: pvar,
    CREATE LATEST ,
    DOES> @ DUP ERR-M !
          DUP C@ 1- SWAP 2 + SWAP
          ROT sendVariable    
;
( ���������� ��� public-����������  ����� ������������ �������������� �������
  �� ���������� � ��������� ������ ���� ����������
)

EXPORT  

: pvar:
  >IN @ NextWord SFIND
  IF 2DROP
  ELSE 2DROP >IN ! 
       pvar,
  THEN
;


: << message: ;

DEFINITIONS

VARIABLE _NVAR
VARIABLE _CURCLASS
VARIABLE _REC
VARIABLE _RECLEN
VARIABLE _OLDCURRENT

EXPORT 

<< :new
<< :free

ALSO ClassContext DEFINITIONS

: [C] \ ���������� current �������� �������� ������
   _CURCLASS @ .methods @ SET-CURRENT
;

: RECORD:
   _NVAR @ DUP _REC ! 
   DEFINITIONS
   CREATE , [C] HERE _RECLEN ! 0 , ['] 2DROP , ['] DROP ,
   DOES> @ self +
;

: /REC
   _NVAR @ _REC @ -
   _RECLEN @ !
;

: size: ( "rec" -- u)
    ' >BODY CELL+ @ LIT,
; IMMEDIATE


\ addr ��������� �� ������� ������ ����������-����������
\ oid - ������� ���������
: _OBJ ( oid addr)
   DUP CELL+ @ :new
   SWAP @ ROT + ! 
;
: _FREE-OBJ ( oid)
   :free
;

: _ARR ( oid addr)
   DUP CELL+ @ DUP ALLOCATE THROW
   DUP ROT ERASE
   SWAP @ ROT + !
;
: _FREE-ARR ( addr)
   FREE THROW
;
( ������ ARR - �������������� � ����.
 - ���������������, ���� ������ ��������� �� :newLit
 � ������� �������� ��������� HERE
 ���������� � OBJ
 - ruv
)

: x-ARR ( len)
   DEFINITIONS
   CREATE
     [C] _NVAR @ ,
     ,
     CELL _NVAR +!
     ['] _ARR ,
     ['] _FREE-ARR ,
   DOES> @ self + @
;

: x-OBJ ( cid)
   DEFINITIONS
   CREATE
     [C] _NVAR @ ,
     ,
     CELL _NVAR +!
     ['] _OBJ ,
     ['] _FREE-OBJ ,
   DOES> @ self + @
;

: ARR
   x-ARR
;
: OBJ
   x-OBJ
;
      
: VAR ( u)
   DEFINITIONS
   CREATE
     \ [C]
     _NVAR @ ,
     DUP ,
     _NVAR +!
     ['] 2DROP ,
     ['] DROP ,
     [C]  \ ������� ������� methods ������ CURRENT -��������.
   DOES> @ self +
;

: InitObj ( oid oid -- oid )
     @ .variables @ @
     BEGIN
       DUP
     WHILE
       2DUP
       NAME> >BODY DUP CELL+
       CELL+ @ EXECUTE ( oid name  oid body xt -- oid name )
       CDR
     REPEAT DROP
;

: <SUPER ( "name" )
   _CURCLASS @ >R
   ' >BODY
   DUP .size @ _NVAR !
   DUP .methods @ @ R@ .methods @ !  \ ��������� �����
   DUP .variables @ @ R@ .variables @ !
   R> .parent !
;

[DEFINED] QuickSWL [IF]
GET-CURRENT ALSO OO_Support ALSO ClassContext DEFINITIONS
: <SUPER
  <SUPER
  _CURCLASS @ DUP
  .methods   @ REFRESH-WLHASH
  .variables @ REFRESH-WLHASH
;
PREVIOUS PREVIOUS SET-CURRENT

[THEN]

: ;CLASS PREVIOUS PREVIOUS
         _OLDCURRENT @ SET-CURRENT
         _NVAR @ _CURCLASS @ .size !
;

WARNING @ WARNING 0!

: :
  >IN @
  NextWord SFIND  IF 
  DUP 1+ @ + message_does = IF
  WARNING @
  WARNING 0! SWAP >IN ! :
  WARNING !                 ELSE
  >IN ! :                   THEN
                  ELSE 
  2DROP >IN ! :   THEN
\+ !CSP !CSP
;

: __:
( ����� ������ �������� �� ������� )
  WARNING @ >R
  >IN @ >R NextWord R> >IN !
  SFIND IF  DUP 1+ @ + message_does = IF WARNING 0! THEN ELSE 2DROP THEN 
  :
  >R WARNING !
;


: abstract S" You can't call abstract method!" ER-U ! ER-A ! -2 THROW ;

: x: 
   : POSTPONE abstract
;

WARNING !

( CONTEXT:  ... OO_Support ClassContext \ top )
PREVIOUS DEFINITIONS

0 VALUE CurrClass

EXPORT

: CLASS: ( - )
   GET-CURRENT _OLDCURRENT !
   ALSO ClassContext
   WORDLIST DUP ALSO CONTEXT !  \ wid of variables
   WORDLIST DUP                 \ wid of methods
   CREATE
     LATEST SWAP SET-CURRENT
     HERE >R /class ALLOT
     R@ /class ERASE
     R@ R@ !
     R@ _CURCLASS !
     R@ .name !
     R@ .methods !
     R@ .variables !
     R> TO CurrClass
;

: own_old
    ?COMP
    CONTEXT @
    GET-CURRENT CONTEXT !
    [COMPILE] '  COMPILE,
    CONTEXT !
; IMMEDIATE

: own
\    CONTEXT @ >R
\    GET-CURRENT CONTEXT ! '
\    R> CONTEXT !

\    NextWord GET-CURRENT SEARCH-WORDLIST
     \ ���� �� ������, �.�. � CURRENT ����� ���� �����-���� Private
    NextWord
    CurrClass .methods @ SEARCH-WORDLIST
    0= IF -321 THROW THEN
    STATE @ 0= IF EXECUTE ELSE COMPILE, THEN
; IMMEDIATE

\ from micro
: >CLASS ' >BODY .methods @ CONTEXT ! ;

: M:: ( c "WM_..." -- )
  \ ���������� ���������� ���������
  \ c - ������ ���� ���������
  BASE @ >R 
  NextWord EVALUATE HEX \ ��� ���� ����� Windows ��������� ��������
  0 <# # # # #  # # # # ROT HOLD BL HOLD [CHAR] : HOLD #>
  EVALUATE
  R> BASE !
;

: W: [CHAR] W M:: ; \ WM_...
: C: [CHAR] C M:: ; \ WM_COMMAND 
: N: [CHAR] N M:: ; \ WM_NOTIFY  
: P: [CHAR] P M:: ; \ WM_PARENTNOTIFY
: M: [CHAR] M M:: ; \ ����

: SearchWM ( mess_id oid c -- xt -1 | 0)
  ROT BASE @ >R HEX
  0 <# # # # #  # # # # ROT HOLD #>
  ROT @ .methods @ SEARCH-WORDLIST
  R> BASE !
;

: ->WM ( mess_id oid c)
\ ������� �������� ��������� �������
  OVER >R SearchWM
  IF R> ExecuteMethod
  ELSE R> DROP
  THEN
;

: WM:
   [CHAR] W ->WM
;

<< :unknown
<< :see
<< :name
<< :length
<< :super
<< :free
<< :new
<< :newLit
<< :methods
<< :variables
<< :init


CLASS: Object

   CELL VAR vClassID

: :length ( - u)
     self class len
;

\ ����� - ��������
: :init
;

: :new ( - oid)
  ." :new="
     self class len DUP
   DUP H.
 ALLOCATE THROW  CELL+
." A| "  DUP H.
     DUP ROT ERASE
     self OVER ! DUP TO self
     DUP @ OVER CELL-
." | " 2DUP H.   H.

 !
." | " DUP H.
     DUP InitObj ( oid oid -- oid )
     self
." |" DUP H.
 :init
;

: :newLit ( - oid)
     self class len HERE OVER ALLOT
     DUP ROT ERASE
     self OVER ! DUP TO self
     DUP InitObj
     self :init
;

: :free 
     self @ .variables @ @
     BEGIN
       DUP
     WHILE
       DUP NAME> >BODY
       DUP @ self + @ \ �������� ���������
       SWAP 3 CELLS + @ EXECUTE
       CDR
     REPEAT DROP 
     self ORG-ADDR <
     self HERE > OR
     IF self CELL- FREE THROW THEN
;

: :super
     self class .parent @
;

: :name ( - addr u)
     self class .name @ COUNT
;

: :unknown
     ." Unknown message " ERR-M @ COUNT TYPE
     ."  for class " own :name TYPE SPACE CR
     ABORT
;

: :see ( c-addr u - addr true | false )
    self class .variables @
    SEARCH-WORDLIST
    IF >BODY @ self + TRUE
    ELSE FALSE
    THEN
;

: :methods
\ ����������� ������ ������
   self class .methods @ NLIST
;
: :variables
\ ����������� ���������� ������
  self class .variables @ NLIST
;  

;CLASS


: METHODS{ ( oid -- )
( C: -- oid1 )
( order: -- methods_wid )
  self SWAP
  DUP TO self
  class .methods @
  ALSO CONTEXT !
;
: }METHODS
( C: oid1 -- )
( order: methods_wid -- )
  TO self
  PREVIOUS
;
: VARS{ ( oid -- )
( C: -- oid1 )
( order: -- vars_wid )
  self SWAP
  DUP TO self
  class .variables @
  ALSO CONTEXT !
;
: }VARS
( C: oid1 -- )
( order: vars_wid -- )
  TO self
  PREVIOUS
;
: EXPAND-CLASS ( oid -- )  ( C: -- oid1 oid2 wid )
( order: -- vars_wid methods_wid )
\ current: -- methods_wid
  DUP >R VARS{ R> METHODS{  GET-CURRENT DEFINITIONS
; IMMEDIATE
: ;EXPAND-CLASS ( C: oid1 oid2 wid -- )
( order: vars_wid methods_wid -- )
\ current: -- wid
  SET-CURRENT
  }METHODS }VARS
;

;MODULE

HERE SWAP -
DROP \ .( Length of Just OOP is ) . .( bytes) CR
