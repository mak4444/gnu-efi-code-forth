
CREATE EFI_SIMPLE_TEXT_INPUT_EX_PROTOCOL_GUID 0xdd9e7534 L, 0x7762 W, 0x4698 W, 0x8c C, 0x14 C, 0xf5 C, 0x85 C, 0x17 C, 0xa6 C, 0x25 C, 0xaa C,

 STRUCTURES{

\ _EFI_SIMPLE_TEXT_INPUT_EX_PROTOCOL
  8  \ *FIELD OI.Reset
	*FIELD ReadKeyStrokeEx
	*FIELD WaitForKeyEx
	*FIELD KX_SetState
	*FIELD RegisterKeyNotify
	*FIELD UnregisterKeyNotify
 DROP

 0
	2 FIELD	ScanCode
	2 FIELD	UnicodeChar
	4 FIELD	KeyShiftState
	1 FIELD	KeyToggleState
7 + 7 ANDC
CONSTANT EFI_KEY_DATA

}STRUCTURES


VARIABLE TEXTINPUEX

  TEXTINPUEX
  EFI_SIMPLE_TEXT_INPUT_EX_PROTOCOL_GUID
  SYSTAB ConsoleInHandle @
  BOOTSERV HandleProtocol @ 3XSYS THROW 

VARIABLE IVANTID
CREATE KEYDATA EFI_KEY_DATA ALLOT $665544332211 ,

: SHIFT+ $100 OR ;
: CTL+   $200 OR ;
: ALT+   $400 OR ;


: KEYEX
  IVANTID
  TEXTINPUEX @ WaitForKeyEx
  1
  BOOTSERV WaitForEvent @ 3XSYS THROW 

  KEYDATA  TEXTINPUEX @ DUP ReadKeyStrokeEx @ 2XSYS THROW 

;

: KEY?EX ( -- flg )
  TEXTINPUEX @ WaitForKeyEx @
  BOOTSERV CheckEvent @ 1XSYS 0=
;

: SHIFT@ KEYDATA KeyShiftState C@ $3 AND ;
: CTL@ KEYDATA KeyShiftState C@ $C AND ;
: ALT@ KEYDATA KeyShiftState C@ $60 AND ;

' KEY?EX TO KEY?

: KEYEX  ( -- key )
  CURSOR
  IVANTID
  TEXTINPUEX @ WaitForKeyEx
  1
  BOOTSERV WaitForEvent @ 3XSYS THROW 
  KEYDATA  TEXTINPUEX @ DUP ReadKeyStrokeEx @ 2XSYS THROW 
  KEYDATA  UnicodeChar W@ 
  KEYDATA  W@  16 << OR
  SHIFT@ IF SHIFT+ THEN
  CTL@ IF CTL+ THEN
  ALT@ IF ALT+ THEN
   CURSOR
 ;

' KEYEX TO KEY

