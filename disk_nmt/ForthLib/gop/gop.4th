

CREATE EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID 0x9042a9de L, 0x23dc W, 0x4a38 W, 0x96 C, 0xfb C, 0x7a C, 0xde C, 0xd0 C, 0x80 C, 0x51 C, 0x6a  C,

 VARIABLE &GOP

 &GOP 0 EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID BOOTSERV LocateProtocol @ 3XSYS THROW

STRUCTURES{

\ EFI_GRAPHICS_OUTPUT_PROTOCOL_MODE
 0
  4 FIELD	GM_MaxMode
  4 FIELD	GM_Mode
  *FIELD	GM_Info
  8 FIELD	GM_SizeOfInfo
  *FIELD	GM_FrameBufferBase
  8 FIELD	GM_FrameBufferSize
DROP

\ _EFI_GRAPHICS_OUTPUT_PROTOCOL
 0
  *FIELD GR_QueryMode
  *FIELD GR_SetMode
  *FIELD GR_Blt
  *FIELD GR_Mode
DROP


\ EFI_GRAPHICS_OUTPUT_MODE_INFORMATION;
0
  4 FIELD	GI_Version
  4 FIELD	HorizontalResolution
  4 FIELD	VerticalResolution
\    PixelFormat
\  EFI_PIXEL_BITMASK          PixelInformation;
\  UINT32                     PixelsPerScanLine;
 DROP

}STRUCTURES
 
 &GOP @ GR_Mode @ GM_FrameBufferBase @ VALUE GR_BASE_ADDRES
 &GOP @ GR_Mode @ GM_Info @ HorizontalResolution L@ VALUE GR_WIDTH
 &GOP @ GR_Mode @ GM_Info @ VerticalResolution L@ VALUE GR_HEIGHT
