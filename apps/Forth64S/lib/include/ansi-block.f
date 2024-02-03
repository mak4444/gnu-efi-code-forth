\ $Id: ansi-block.f,v 1.3 2008/02/27 13:57:34 ruv Exp $
\ 
\ BLOCK BLOCK-EXT
\ 
\ by Forthware 
\ http://fforum.winglion.ru/viewtopic.php?p=12991#12991

MODULE: BLK-SUPPORT

CREATE $BUFFERS 2500 ALLOT \ ����� �������� ������ � �� �������, �������, ����� $buffers ��������� �� ������ 
$BUFFERS 2500 ERASE $BUFFERS DUP 100 + DUP 1100 + ROT 2! 

CREATE $STORAGE 0 , 
: STORAGE-NAME S" storage1.blk" ; 
: OPEN-STORAGE ( -- ) 
 STORAGE-NAME R/W OPEN-FILE 
 IF DROP STORAGE-NAME R/W CREATE-FILE THROW THEN 
 $STORAGE ! 
; 
: CLOSE-STORAGE ( -- ) $STORAGE @ CLOSE-FILE THROW ; 


EXPORT

: SAVE-BUFFER ( addr -- ) 
  DUP 2 CELLS - @ 1 AND 
  IF 
  OPEN-STORAGE 
  DUP 1 CELLS - @ 1- 1024 M* $STORAGE @ REPOSITION-FILE IF CLOSE-STORAGE -35 THROW THEN 
  DUP 1024 $STORAGE @ WRITE-FILE IF CLOSE-STORAGE -34 THROW THEN 
  CLOSE-STORAGE 
  2 CELLS - 2 SWAP ! 
  ELSE 
   DROP 
  THEN 
; 
: BUFFER ( u -- addr ) 
 $BUFFERS >R 
 R@ @ 1 CELLS - @ OVER = IF DROP R> @ EXIT THEN 
 R@ 2@ SWAP R@ 2! 
 R@ @ 1 CELLS - @ OVER = IF DROP R> @ EXIT THEN 
 R> @ DUP >R SAVE-BUFFER 
 R@ 1 CELLS - ! 0 R@ 2 CELLS - ! R> 
; 
: BLOCK ( u -- addr ) 
 BUFFER DUP 2 CELLS - @ 2 AND 0= 
 IF 
  OPEN-STORAGE 
  DUP 1 CELLS - @ 1- 1024 M* $STORAGE @ REPOSITION-FILE IF CLOSE-STORAGE -35 THROW THEN 
  DUP 1024 $STORAGE @ READ-FILE IF CLOSE-STORAGE -33 THROW THEN DROP 
  DUP 2 CELLS - 2 SWAP ! 
  CLOSE-STORAGE 
 THEN 
; 

: SAVE-BUFFERS ( -- ) $BUFFERS 2@ SAVE-BUFFER SAVE-BUFFER ; 
: FLUSH ( -- ) SAVE-BUFFERS $BUFFERS 2@ 2 CELLS - 2 CELLS ERASE 2 CELLS - 2 CELLS ERASE ; 
: UPDATE ( -- ) $BUFFERS @ 2 CELLS - DUP @ 3 OR SWAP ! ; 
: EMPTY-BUFFERS ( -- ) $BUFFERS 2@ 2 CELLS - 1032 ERASE 2 CELLS - 1032 ERASE ; 
VARIABLE SCR 
: LIST ( u -- ) 
 DUP SCR ! BLOCK 17 1 
 DO 
  CR I 0 <# # # #> TYPE SPACE 64 0 DO DUP C@ BL MAX EMIT CHAR+ LOOP ." \" 
 LOOP DROP 
; 
: LOAD ( i*x u � j*x ) 
  DUP 0= IF -35 ( invalid block number ) THROW THEN \ �������� ����� �� ���������� 

  BLK @ >IN @ SOURCE 4 N>R \ ��������� ������ �������� (������ ��, ��� ���������)
  DUP BLK ! BLOCK 1024 SOURCE! \ ������������� ����� ��������
  ['] INTERPRET CATCH DUP IF \ ������ ���� ����� ��������� �� ������
   CR ." BLOCK " BLK @ . ." , LINE " >IN @ 1- 64 / DUP 1+ . ." :" CR 
   SOURCE DROP SWAP 64 * + >IN @ 1- 64 MOD OVER + SWAP 
   ?DO I C@ BL MAX EMIT LOOP SPACE ." ERROR #" DUP . CR
  THEN ( ior )
  NR> DROP SOURCE! >IN ! BLK !  THROW
;

: THRU ( i*x u1 u2 -- j*x ) 1+ SWAP ?DO I LOAD LOOP ;

: EVALUATE BLK @ >R 0 BLK ! ['] EVALUATE CATCH R> BLK ! THROW ;
: REFILL_BLK BLK @ IF 1 BLK +! 0 >IN ! TRUE ELSE [ ' REFILL BEHAVIOR COMPILE, ] THEN ; ' REFILL_BLK TO REFILL
: \ BLK @ IF >IN @ 63 + -64 AND >IN ! ELSE POSTPONE \ THEN ; IMMEDIATE 


;MODULE
