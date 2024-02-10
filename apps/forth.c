#include <efi.h>
#include <efilib.h>
#include <eficonex.h>

//int   DPBuff[0x10];
//int   _Stekc[0x10];
static EFI_STATUS
SetWatchdog(UINTN seconds)
{
	EFI_STATUS rc;
	rc = uefi_call_wrapper(BS->SetWatchdogTimer, 4, seconds, 0x1ffff,
				0, NULL);
	if (EFI_ERROR(rc)) {
		CHAR16 Buffer[64];
		StatusToString(Buffer, rc);
		Print(L"Bad response from QueryMode: %s (%d)\n", Buffer, rc);
	}
	return rc;
}

VOID
C_HALT( EFI_STATUS stat );

VOID
C_HALT( EFI_STATUS stat )
{	Exit(stat, 0, NULL);
}

EFI_FILE_HANDLE GetVolume(EFI_HANDLE image)
{
  EFI_LOADED_IMAGE *loaded_image = NULL;                  /* image interface */
  EFI_GUID lipGuid = EFI_LOADED_IMAGE_PROTOCOL_GUID;      /* image interface GUID */
  EFI_FILE_IO_INTERFACE *IOVolume;                        /* file system interface */
  EFI_GUID fsGuid = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID; /* file system interface GUID */
  EFI_FILE_HANDLE Volume;                                 /* the volume's interface */
 
  /* get the loaded image protocol interface for our "image" */
  uefi_call_wrapper(BS->HandleProtocol, 3, image, &lipGuid, (void **) &loaded_image);
  /* get the volume handle */
  uefi_call_wrapper(BS->HandleProtocol, 3, loaded_image->DeviceHandle, &fsGuid, (VOID*)&IOVolume);
  uefi_call_wrapper(IOVolume->OpenVolume, 2, IOVolume, &Volume);
  return Volume;
}
EFI_FILE_HANDLE Volume;

UINT64 GCCFileSize(EFI_FILE_HANDLE FileHandle)
{
  UINT64 ret;
  EFI_FILE_INFO       *FileInfo;         /* file information structure */
  /* get the file's size */
  FileInfo = LibFileInfo(FileHandle);
  ret = FileInfo->FileSize;
  FreePool(FileInfo);
  return ret;
}

EFI_STATUS
GCCOpenFile(EFI_FILE_HANDLE * FileHandle, CHAR16 *FileName, UINT64 flg )
{   
   return uefi_call_wrapper(Volume->Open, 5, Volume, FileHandle, FileName, flg , 0 );
}

EFI_STATUS
GCCReadFile(EFI_FILE_HANDLE FileHandle,UINT64 * ReadSize, VOID *Buffer )
{
 return uefi_call_wrapper(FileHandle->Read, 3, FileHandle, ReadSize, Buffer);
}

EFI_STATUS
GCCWriteFile(EFI_FILE_HANDLE FileHandle,UINT64 * WriteSize, VOID *Buffer )
{
 return uefi_call_wrapper(FileHandle->Write, 3, FileHandle, WriteSize, Buffer);
}

EFI_STATUS
GCCSetPosition(EFI_FILE_HANDLE FileHandle, UINTN Offset )
{
   return uefi_call_wrapper(FileHandle->SetPosition, 2, FileHandle, Offset);
}

EFI_STATUS
GCCGetPosition(EFI_FILE_HANDLE FileHandle, UINTN * Offset )
{
   return uefi_call_wrapper(FileHandle->GetPosition, 2, FileHandle, Offset);
}

EFI_STATUS
GCCCloseFile(EFI_FILE_HANDLE FileHandle)
{ return uefi_call_wrapper(FileHandle->Close, 1, FileHandle);
}

VOID
GCCSetAttribute(UINTN color )
{	uefi_call_wrapper(ST->ConOut->SetAttribute, 2, ST->ConOut, color);
}

VOID
GCCSetXY(UINTN Row,UINTN  Column)
{
   uefi_call_wrapper(ST->ConOut->SetCursorPosition, 3, ST->ConOut, Column, Row);
}

VOID
GCCOUTPUTRESET()
{
   uefi_call_wrapper(ST->ConOut->Reset, 2, ST->ConOut,0);
}

SIMPLE_TEXT_OUTPUT_MODE *
GCCTEXT_OUTPUT_MODE()
{  return ST->ConOut->Mode;
}
EFI_STATUS 
GCCSCRSIZE(OUT UINTN *TempColumn, OUT UINTN *ScreenSize)
{
  return uefi_call_wrapper(ST->ConOut->QueryMode, 4, ST->ConOut, ST->ConOut->Mode->Mode, TempColumn, ScreenSize);
}

typedef struct {
    CHAR16                              UnicodeChar;
    UINT16                              ScanCode;
} EFI_INPUT_KEY_M;


EFI_INPUT_KEY_M
getkey ()
{
	EFI_INPUT_KEY_M efi_input_key_M;
	EFI_INPUT_KEY efi_input_key;
	UINTN index;
	uefi_call_wrapper(ST->BootServices->WaitForEvent, 3, 1, &ST->ConIn->WaitForKey, &index);
	uefi_call_wrapper(ST->ConIn->ReadKeyStroke, 2, ST->ConIn, &efi_input_key);
	efi_input_key_M.UnicodeChar= efi_input_key.UnicodeChar;
	efi_input_key_M.ScanCode= efi_input_key.ScanCode;
	return efi_input_key_M;
}

VOID
start4th_m(VOID *);

EFI_STATUS
efi_main (EFI_HANDLE image_handle, EFI_SYSTEM_TABLE *systab)
{	EFI_GUID loaded_image_protocol = LOADED_IMAGE_PROTOCOL;
	EFI_STATUS efi_status;
	EFI_LOADED_IMAGE *li;
	VOID *void_li_p;

	InitializeLib(image_handle, systab);
        Volume = GetVolume(image_handle);
	/*
	 * Locate loaded_image_handle instance.
	 */

	Print(L"BS->HandleProtocol()  ");

	efi_status = uefi_call_wrapper(
		BS->HandleProtocol,
		3,
		image_handle,
		&loaded_image_protocol,
		&void_li_p);
	li = void_li_p;

	Print(L"%xh (%r)\n", efi_status, efi_status);

	Print(L"  li: %xh\n", li);

	Print(L"  li->FilePath:        %s\n", li->FilePath);

//        testHotKey();
	uefi_call_wrapper(ST->ConOut->EnableCursor, 2, ST->ConOut, 1);
	Print(L"Forth application started\n");
	SetWatchdog(0);
//	WaitForSingleEvent(ST->ConIn->WaitForKey, 0);
//	uefi_call_wrapper(ST->ConOut->OutputString, 2, ST->ConOut, L"\n\n");
	VOID * DPBuff_adr =  AllocatePool(0x180000);
		start4th_m(DPBuff_adr);
		pek();
	return EFI_SUCCESS;
}
