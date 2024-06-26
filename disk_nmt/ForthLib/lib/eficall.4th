

: USCAN ( c-addr1 u1 w --- c-addr2 u2 )
\ Find the first occurrence of unicode character c in the string c-addr1 u1
\ c-addr2 u2 is the remaining part of the string starting with that uchar.
\ It is a zero-length string if c was not found.
  BEGIN
   OVER
  WHILE
   ROT DUP W@ >R OVER R> =
   IF
    ROT ROT DROP EXIT
   THEN
   2+ ROT ROT SWAP 2- SWAP
  REPEAT DROP
;

\- UZCOUNT : UZCOUNT ( zaddr -- zaddr n )   dup dup if  65535 0 USCAN drop over - then ;

ALIGN
CREATE EFI_DEVICE_PATH_PROTOCOL_GUID		0x09576E91 L, 0x6D3F W, 0x11D2 W,	0x8E C, 0x39 C, 0x0 C, 0xA0 C, 0xC9 C, 0x69 C, 0x72 C, 0x3B C,
CREATE EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID	0x964e5b22 L, 0x6459 W, 0x11d2 W,	0x8e C, 0x39 C, 0x0 C, 0xa0 C, 0xc9 C, 0x69 C, 0x72 C, 0x3b C,
CREATE EFI_DEVICE_PATH_UTILITIES_PROTOCOL_GUID  0x379be4e L, 0xd706 W, 0x437d W,	0xb0 C, 0x37 C, 0xed C, 0xb8 C, 0x2f C, 0xb7 C, 0x72 C, 0xa4 C,

VARIABLE DEVICE_PATH_UTILITIES

DEVICE_PATH_UTILITIES  0
EFI_DEVICE_PATH_UTILITIES_PROTOCOL_GUID		BOOTSERV LocateProtocol @  3XSYS THROW

[IFNDEF] AppendDevicePath
 STRUCTURES{
0
    *FIELD GetDevicePathSize
    *FIELD DuplicateDevicePath
    *FIELD AppendDevicePath
    *FIELD AppendDeviceNode
    *FIELD AppendDevicePathInstance
    *FIELD GetNextDevicePathInstance
    *FIELD IsDevicePathMultiInstance
    *FIELD CreateDeviceNode
DROP

 }STRUCTURES

[THEN]

\- MEDIA_DEVICE_PATH	4 CONSTANT MEDIA_DEVICE_PATH  
\- MEDIA_FILEPATH_DP	4 CONSTANT MEDIA_FILEPATH_DP

CREATE C_FILE_PATH 
  MEDIA_DEVICE_PATH C, MEDIA_FILEPATH_DP C,
 0 W, \ size
 $222 ALLOT \ FileName

 STRUCTURES{
\ EFI_DEVICE_PATH_PROTOCOL
0
        1 FIELD    DEVType
        1 FIELD    DEVSubType
        2 FIELD    DEVLength
 CONSTANT DEVHAED_SIZE
 }STRUCTURES

VARIABLE DEV_PATH
VARIABLE DEVI_PATH
VARIABLE DEVPATH
VARIABLE DEV_HEAD
VARIABLE LOADHANDLE

: SETDEVICE ( n -- flg )
  DEV_PATH
  EFI_DEVICE_PATH_PROTOCOL_GUID
  ROT CELLS vol_handles @ + @		BOOTSERV HandleProtocol @ 3XSYS

 DUP IF BREAK DROP
 DEV_HEAD  DEV_PATH
 EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID	BOOTSERV LocateDevicePath @ 3XSYS 
;

: >CALLREFIFILENAME ( adr1 len adr -- adr )
  2 PICK 
  1+ C@ '\' =
  IF   2 PICK C@ '0' XOR SETDEVICE THROW
	>R 2- SWAP 2+ SWAP R> ASCII-UZ
  BREAK
 CUR_DIR COUNT + 1- C@ '\' <> IF '\' CUR_DIR $C+! THEN
 CUR_DIR 2+ C@ '\' = IF CUR_DIR 1+ C@ '0' XOR SETDEVICE DUP IF $3001 CUR_DIR W! THEN THROW THEN
 CUR_DIR COUNT 1- SWAP 1+ SWAP ROT ASCII-UZ \  adr1 len adr
 DUP>R CUR_DIR C@ 1- 2* + ASCII-UZ DROP R> Z/TO\
 ;

: EFIC_SETNAME ( adr lem -- )
 C_FILE_PATH DEVHAED_SIZE +   >CALLREFIFILENAME
 UZCOUNT 2+ 2DUP + $4FF7F SWAP L!
 4+ SWAP 2-  W! ;

: $EFICALL  ( adr lem -- )
 EFIC_SETNAME

 DEVI_PATH 
 EFI_DEVICE_PATH_PROTOCOL_GUID DEV_HEAD @	BOOTSERV HandleProtocol @  3XSYS THROW
 C_FILE_PATH DEVI_PATH @			DEVICE_PATH_UTILITIES @ AppendDevicePath @ 2XSYS
 DEVPATH !

 LOADHANDLE 0 0 DEVPATH @ IMAGEHANDLE FALSE	BOOTSERV LoadImage @ 6XSYS THROW
 0 0 LOADHANDLE @				BOOTSERV StartImage @ 3XSYS THROW
;

: EFICALL PARSE-NAME $EFICALL ; 
