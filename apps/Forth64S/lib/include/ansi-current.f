\ 16.Oct.2007
\ $Id: ansi-current.f,v 1.1 2007/10/16 11:01:05 ruv Exp $

( ��������� ������ CURRENT � �������� ����������� �����;
�� ������ ��� ������� ��������.
  -- ��. Bug#1808325, https://sourceforge.net/tracker/index.php?func=detail&aid=1808325&group_id=17919&atid=117919

������������ � locals.f � ���� �������� �����, ����� � ��������
����������� ����� ��������� ������ ����� � ������ ��������� �������.

��� ������� ������������� ����������� ������ ������ � ��������� �������.
)

WARNING @  WARNING 0!

: SMUDGE ( -- )
\  LAST-NON IF EXIT THEN
  LAST @ IF
    C-SMUDGE C@
    LAST @ 1+ C@ C-SMUDGE C!
    LAST @ 1+ C!
  THEN
;
: HIDE
  12 C-SMUDGE C! SMUDGE
;
: : ( C: "<spaces>name" -- colon-sys ) \ 94
  HEADER ] HIDE
;
: ; ( -- )
  RET, [COMPILE] [ SMUDGE
  ClearJpBuff
  0 TO LAST-NON
; IMMEDIATE

WARNING !
