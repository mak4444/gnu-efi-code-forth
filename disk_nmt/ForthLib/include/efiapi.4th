
0 CONSTANT AllHandles
1 CONSTANT ByRegisterNotify
2 CONSTANT ByProtocol

 STRUCTURES{
 0
	8FIELD Signature
	4FIELD TH_Revision
	4FIELD HeaderSize
	4FIELD CRC32
	4+ \  Reserved
 CONSTANT /EFI_TABLE_HEADER


  0
    /EFI_TABLE_HEADER FIELD ST.Hdr

	*FIELD *FirmwareVendor
	4FIELD FirmwareRevision

	*FIELD ConsoleInHandle
	*FIELD ST*ConIn

	*FIELD ConsoleOutHandle
	*FIELD ST*ConOut

	*FIELD StandardErrorHandle
	*FIELD ST*StdErr

	*FIELD *RuntimeServices
	*FIELD *BootServices

	4FIELD NumberOfTableEntries;
	*FIELD *ConfigurationTable;

 CONSTANT /EFI_SYSTEM_TABLE


0
    /EFI_TABLE_HEADER FIELD     BS_Hdr

    \
    \ Task priority functions
    \

    *FIELD                   RaiseTPL
    *FIELD                 RestoreTPL

    \
    \ Memory functions
    \

    *FIELD AllocatePages
    *FIELD BS_FreePages
    *FIELD GetMemoryMap
    *FIELD BS_AllocatePool
    *FIELD BS_FreePool

    \
    \ Event & timer functions
    \

    *FIELD CreateEvent
    *FIELD BS_SetTimer
    *FIELD WaitForEvent
    *FIELD SignalEvent
    *FIELD CloseEvent
    *FIELD CheckEvent

    \
    \ Protocol handler functions
    \

    *FIELD  InstallProtocolInterface
    *FIELD ReinstallProtocolInterface
    *FIELD UninstallProtocolInterface
    *FIELD             HandleProtocol
    *FIELD             PCHandleProtocol
    *FIELD    RegisterProtocolNotify
    *FIELD               LocateHandle
    *FIELD          LocateDevicePath
    *FIELD InstallConfigurationTable

    \
    \ Image functions
    \

    *FIELD                  LoadImage
    *FIELD                 StartImage
    *FIELD                       BS_Exit
    *FIELD                UnloadImage
    *FIELD          ExitBootServices

    \
    \ Misc functions
    \

    *FIELD    GetNextMonotonicCount
    *FIELD                       BS_Stall
    *FIELD          SetWatchdogTimer

    \
    \ DriverSupport Services
    \

    *FIELD          ConnectController
    *FIELD       DisconnectController

    \
    \ Open and Close Protocol Services
    \
    *FIELD               OpenProtocol
    *FIELD              CloseProtocol
    *FIELD   OpenProtocolInformation

    \
    \ Library Services
    \
    *FIELD        ProtocolsPerHandle
    *FIELD        LocateHandleBuffer
    *FIELD             LocateProtocol
    *FIELD InstallMultipleProtocolInterfaces
    *FIELD UninstallMultipleProtocolInterfaces

    \
    \ 32-bit CRC Services
    \
    *FIELD             CalculateCrc32

    \
    \ Misc Services
    \
    *FIELD                   BS_CopyMem
    *FIELD                   BS_SetMem
    *FIELD             CreateEventEx
DROP

 }STRUCTURES



