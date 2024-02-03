( 28.Mar.2000 Andrey Cherezov  Copyright [C] RU FIG

  Использованы идеи следующих авторов:
  Ruvim Pinka; Dmitry Yakimov; Oleg Shalyopa; Yuriy Zhilovets;
  Konstantin Tarasov; Michail Maximov.

  !! Работает только в SPF4.
)

( Простое расширение СП-Форта локальными переменными.
  Реализовано без использования LOCALS стандарта 94.

  Объявление временных переменных, видимых только внутри
  текущего слова и ограниченных временем вызова данного
  слова выполняется с помощью слова "{". Внутри определения 
  слова используется конструкция, подобная стековой нотации Форта
  { список_инициализированных_локалов \ сп.неиниц.локалов -- что угодно }
  Например:

  { a b c d \ e f -- i j }

  Или { a b c d \ e f[ EVALUATE_выражение ] -- i j }
  Это значит что для переменной f[ будет выделен на стеке возвратов участок
  памяти длиной n байт. Использование переменной f[ даст адрес начала этого
  участка. \В стиле MPE\

  Или { a b c d \ e [ 12 ] f -- i j }
  Это значит что для переменной f будет выделен на стеке возвратов участок
  памяти длиной 12 байт. Использование переменной f даст адрес начала этого
  участка. 

  Часть "\ сп.неиниц.локалов" может отсутствовать, например:

  { item1 item2 -- }

  Это заставляет СП-Форт автоматически выделять место в
  стеке возвратов для этих переменных в момент вызова слова
  и автоматически освобождать место при выходе из него.

  Обращение к таким локальным переменным - как к VALUE-переменным
  по имени. Если нужен адрес переменной, то используется "^ имя"
  или "AT имя".


  Вместо \ можно использовать |
  Вместо -> можно использовать TO

  Примеры:

  : TEST { a b c d \ e f -- } a . b . c .  b c + -> e  e .  f .  ^ a @ . ;
   Ok
  1 2 3 4 TEST
  1 2 3 5 0 1  Ok

  : TEST { a b -- } a . b . CR 5 0 DO I . a . b . CR LOOP ;
   Ok
  12 34 TEST
  12 34
  0 12 34
  1 12 34
  2 12 34
  3 12 34
  4 12 34
   Ok

  : TEST { a b } a . b . ;
   Ok
  1 2 TEST
  1 2  Ok

  : TEST { a b \ c } a . b . c . ;
   Ok
  1 2 TEST
  1 2 0  Ok

  : TEST { a b -- } a . b . ;
   Ok
  1 2 TEST
  1 2  Ok

  : TEST { a b \ c -- d } a . b . c . ;
   Ok
  1 2 TEST
  1 2 0  Ok

  : TEST { \ a b } a . b .  1 -> a  2 -> b  a . b . ;
   Ok
  TEST
  0 0 1 2  Ok

  Имена локальных переменных существуют в динамическом
  временном словаре только в момент компиляции слова, а
  после этого вычищаются и более недоступны.

  Использовать конструкцию "{ ... }" внутри одного определения можно
  только один раз.

  Компиляция этой библиотеки добавляет в текущий словарь компиляции
  Только два слова:
  словарь "vocLocalsSupport" и "{"
  Все остальные детали "спрятаны" в словаре, использовать их
  не рекомендуется.
)

REQUIRE [IF] ~MAK\CompIF.f

MODULE: vocLocalsSupport

VARIABLE uLocalsCnt
VARIABLE uLocalsUCnt
VARIABLE uPrevCurrent
VARIABLE uAddDepth

: LocalOffs ( n -- offs )
  2+ CELLS uAddDepth @ +
;

BASE @ HEX
 

: RALLOT,  ( n --  )
\ CELLS NEGATE LIT,  S"  RP@ + RP! " EVALUATE
 0 DO 0 LIT,  POSTPONE >R LOOP
 ;

: CompileLocalRec ( u -- )
  LocalOffs   POSTPONE LITERAL
  S"  RP@ + " EVALUATE
;

: CompileLocal@ ( n -- )
  CompileLocalRec 
  S" @ " EVALUATE
;

: CompileLocal! ( n -- )
  CompileLocalRec
  S" ! " EVALUATE
;

VARIABLE TEMP-DP
VARIABLE TEMP-LAST

: CompileLocalsInit
  TEMP-DP @ DP ! 
  TEMP-LAST @  LAST !
  uPrevCurrent @ SET-CURRENT
  uLocalsUCnt @ ?DUP
  IF  RALLOT,
  THEN 
  uLocalsCnt @ uLocalsUCnt @ - ?DUP
  IF  0 DO  POSTPONE >R LOOP
 THEN
  uLocalsCnt  @ ?DUP
  IF CELLS LIT,  POSTPONE >R ['] LOCALS_EXIT LIT, POSTPONE >R
  THEN
;


\ : CompileLocal@ ( n -- )
\   LocalOffs LIT, POSTPONE RP+@
\ ;


BASE !

WORDLIST CONSTANT widLocals@

CREATE  TEMP-BUF 1000 ALLOT

: LocalsStartup
  GET-CURRENT uPrevCurrent !
  ALSO vocLocalsSupport
  ALSO widLocals@ CONTEXT ! DEFINITIONS
  HERE TEMP-DP !
  LAST @  TEMP-LAST !
  TEMP-BUF DP ! 
  widLocals@  0!
  uLocalsCnt 0!
  uLocalsUCnt 0!
  uAddDepth 0!
;

: LocalsCleanup
  PREVIOUS PREVIOUS
;


: ProcessLocRec ( "name" -- u )
  [CHAR] ] PARSE
  STATE 0!
  EVALUATE CELL 1- + CELL / \ делаем кратным 4
  -1 STATE ! 
\  DUP uLocalsCnt +!
  uLocalsCnt @
;

: CreateLocArray
  [CHAR] [ PSKIP
  ProcessLocRec
  CREATE ,
  DUP uLocalsCnt +!  
;

: LocalsRecDoes@ ( -- u )
  DOES>  @ CompileLocalRec
;


: LocalsRecDoes@2 ( -- u )
  ProcessLocRec , 
  DUP uLocalsCnt +!
  DOES> @ CompileLocalRec
;

: LocalsDoes@
  uLocalsCnt @ ,
  uLocalsCnt 1+!
  DOES>  @  CompileLocal@
;

: ;; POSTPONE ; ; IMMEDIATE


: ^ 
  ' >BODY @ 
  CompileLocalRec
; IMMEDIATE


: -> ' >BODY @ CompileLocal!  ; IMMEDIATE

WARNING DUP @ SWAP 0!

: AT
  [COMPILE] ^
; IMMEDIATE

: TO ( "name" -- )
  >IN @ PARSE-NAME widLocals@ SEARCH-WORDLIST 1 =
  IF >BODY @ CompileLocal! DROP
  ELSE >IN ! [COMPILE] TO
  THEN
; IMMEDIATE

WARNING !

WARNING @ WARNING 0!
\ ===
\ переопределение соответствующих слов для возможности использовать
\ временные переменные внутри  цикла DO LOOP  и независимо от изменения
\ содержимого стека возвратов  словами   >R   R>
C" DO_SIZE" FIND NIP 0=
[IF] 3 CELLS CONSTANT DO_SIZE
[THEN]


: DO    POSTPONE DO      DO_SIZE              uAddDepth +! ; IMMEDIATE
: ?DO   POSTPONE ?DO     DO_SIZE              uAddDepth +! ; IMMEDIATE
: LOOP  POSTPONE LOOP    DO_SIZE NEGATE       uAddDepth +! ; IMMEDIATE
: +LOOP POSTPONE +LOOP   DO_SIZE NEGATE       uAddDepth +! ; IMMEDIATE
: >R    POSTPONE >R     [  1 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
: R>    POSTPONE R>     [ -1 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
: RDROP POSTPONE RDROP  [ -1 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
: 2>R   POSTPONE 2>R    [  2 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
: 2R>   POSTPONE 2R>    [ -2 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE


\ ===

: ;  LocalsCleanup
    S" ;" EVALUATE
; IMMEDIATE

WARNING !

\ =====================================================================


EXPORT

: {
  
  LocalsStartup
  BEGIN
    BL PSKIP PeekChar DUP [CHAR] \ <> 
                    OVER [CHAR] - <>  AND
                    OVER [CHAR] } <>  AND
                    OVER [CHAR] | <>  AND
                    SWAP [CHAR] ) XOR AND
  WHILE

    CREATE  LocalsDoes@ IMMEDIATE
  REPEAT
  PeekChar >IN 1+! DUP [CHAR] } <>
  IF
     DUP [CHAR] \ =
    SWAP [CHAR] | = OR
    IF
      BEGIN
        BL PSKIP PeekChar DUP 
         DUP [CHAR] - <> 
        SWAP [CHAR] } <>  AND
        SWAP [CHAR] ) XOR AND
      WHILE
        PeekChar [CHAR] [ =
        IF  CreateLocArray  LocalsRecDoes@
        ELSE
             CREATE LATEST DUP C@ + C@
             [CHAR] [ =
             IF  
               LocalsRecDoes@2
             ELSE
               LocalsDoes@ 1
             THEN
        THEN        uLocalsUCnt +!
        IMMEDIATE
      REPEAT
    THEN
    [CHAR] } PARSE 2DROP
  ELSE DROP THEN
  CompileLocalsInit
;; IMMEDIATE

;MODULE
