\ ��������������� ����� ��� ������ ��� ��� ���� ������� ���
\ Win32


: TryOpenFile ( addr u mode -- u ior | handle 0 )
 \ ���� ���������� ����, ���� �������� module path ���� � /devel
    >R
    2DUP R@ OPEN-FILE-SHARED
    IF DROP 2DUP
       +ModuleDirName
       R@ OPEN-FILE-SHARED
       IF DROP +LibraryDirName
          R@ OPEN-FILE-SHARED
       ELSE NIP NIP 0
       THEN
    ELSE NIP NIP 0
    THEN R> DROP
;