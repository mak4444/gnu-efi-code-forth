CODE _RWIN-CODE2
     LEA     EBP, -8 CELL- [EBP] 
     POP     EBX 
     POP     EDX 
     MOV     EAX, [EBX] 
     OR      EAX, EAX 
     MOV     4 [EBP], EDX 
     JNZ  SHORT @@1

     CALL    ' AO_INI

     JZ  SHORT @@2

     MOV     [EBX], EAX 

@@1: MOV     0 [EBP], EDI 
     CALL    EAX 
     MOV     EDI, 0 [EBP] 
@@2:
     LEA     EBP, 8 CELL+ [EBP] 
 JMP     -8 [EBP]

END-CODE

\ : 0/ 0 / ;


: RWIN:  ( "RИмяПроцедуры" "ИмяБиблиотеки" -- )
  0
  >IN @  WHEADER
  1+ >IN !
 ['] _RWIN-CODE2  __WIN: ;

RWIN: RGetModuleFileNameA            KERNEL32.DLL
RWIN: RGetModuleHandleA KERNEL32.DLL
RWIN: RGetCommandLineA KERNEL32.DLL
RWIN: RWinExec      KERNEL32.DLL
RWIN: Rlstrcmp KERNEL32.DLL
RWIN: RLoadLibraryA                  KERNEL32.DLL

RWIN: RFindFirstFileA KERNEL32.DLL
\ RWIN: RFindNextFileA  KERNEL32.DLL
RWIN: RFindClose      KERNEL32.DLL
RWIN: RWriteFile                     KERNEL32.DLL
RWIN: RReadFile                      KERNEL32.DLL
RWIN: RCloseHandle                   KERNEL32.DLL
RWIN: RGetFileSize                   KERNEL32.DLL


RWIN: RCreatePopupMenu  USER32.DLL
RWIN: RCreateMenu  USER32.DLL
RWIN: RAppendMenuA  USER32.DLL
RWIN: RMoveWindow     USER32.DLL
RWIN: RUpdateWindow     USER32.DLL
RWIN: RShowWindow       USER32.DLL
RWIN: RDestroyWindow        USER32.DLL
RWIN: RGetSystemMetrics USER32.DLL
RWIN: RRegisterClassA   USER32.DLL
RWIN: RRegisterClassExA   USER32.DLL
RWIN: RLoadCursorA       USER32.DLL
RWIN: RLoadIconA        USER32.DLL
RWIN: RLoadMenuA        USER32.DLL
RWIN: RSetMenu        USER32.DLL
RWIN: RDefWindowProcA   USER32.DLL
RWIN: RGetClientRect    USER32.DLL
RWIN: RSetFocus         USER32.DLL
RWIN: RSendMessageA	USER32.DLL

\ samples\~mak\WIN\ED\ed.f \EOF
RWIN: RMessageBoxA             USER32.DLL
RWIN: RGetWindowTextLengthA USER32.DLL

RWIN: RGetMessageA      USER32.DLL
RWIN: RCreateWindowExA      USER32.DLL
RWIN: RGetDlgItem           USER32.DLL
RWIN: REndDialog            USER32.DLL
RWIN: RDialogBoxParamA      USER32.DLL
RWIN: RGetCursorPos         USER32.DLL
RWIN: RGetSubMenu USER32.DLL
RWIN: RTrackPopupMenu        USER32.DLL
RWIN: RCallWindowProcA      USER32.DLL
RWIN: RSetWindowLongA       USER32.DLL


RWIN: RTranslateMessage     USER32.DLL
RWIN: RDispatchMessageA     USER32.DLL
RWIN: RPostQuitMessage  USER32.DLL


: SetWindowText ABORT ; IMMEDIATE

WINAPI:  SetWindowTextA       USER32.DLL
 RWIN: RSetWindowTextA       USER32.DLL
RWIN: RGetWindowTextA       USER32.DLL


RWIN: ROpenFile        KERNEL32.DLL
RWIN: RCreateFileA                   KERNEL32.DLL


RWIN: RInitCommonControlsEx COMCTL32.DLL
RWIN: RInitCommonControls	COMCTL32.DLL
RWIN: RGetOpenFileNameA		COMDLG32.DLL


RWIN: RCreateCompatibleDC GDI32.DLL
RWIN: RSelectObject       GDI32.DLL
RWIN: RDeleteObject       GDI32.DLL
RWIN: RDeleteDC           GDI32.DLL
RWIN: RGetSysColor        USER32.DLL
RWIN: RGetPixel           GDI32.DLL
RWIN: RExtFloodFill       GDI32.DLL
RWIN: RCreateSolidBrush             GDI32.DLL
RWIN: RGetStockObject   GDI32.DLL

RWIN: RLoadBitmapA  user32.dll


RWIN: RCreateStatusWindow COMCTL32.DLL
RWIN: RSysFreeString     OLEAUT32.DLL
RWIN: RSysAllocStringByteLen oleaut32.dll
RWIN: RShellAboutA          shell32.dll
RWIN: RDragQueryFileA       shell32.dll
RWIN: RPathGetArgsA shlwapi.dll
RWIN: RPathUnquoteSpacesA   shlwapi.dll
