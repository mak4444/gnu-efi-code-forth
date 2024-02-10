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

EFI_HANDLE notifyHandle;

EFI_KEY_DATA *Gkey;
EFI_STATUS
myNotify(IN EFI_KEY_DATA *key)
{        Gkey=key;
//	Print((CONST CHAR16*)L"Hot Key %x\n" , key->KeyState.KeyShiftState);
	return 0;
}


EFI_STATUS 
testHotKey()
{
	EFI_STATUS  Status;
	EFI_KEY_DATA hotkey={0};
	EFI_KEY_DATA key = {0};
	EFI_SIMPLE_TEXT_INPUT_EX_PROTOCOL* InputEx = NULL;
	Status = uefi_call_wrapper(BS->LocateProtocol,3, &SimpleTextInputExProtocol,
			NULL,(VOID**)&InputEx );

	Print(L"LocateProtocol=%r\n", Status);

	hotkey.Key.ScanCode = 0;
	hotkey.Key.UnicodeChar = 'c';
	hotkey.KeyState.KeyShiftState = EFI_LEFT_CONTROL_PRESSED | EFI_SHIFT_STATE_VALID;
	hotkey.KeyState.KeyToggleState = EFI_TOGGLE_STATE_VALID;


	Status = InputEx->RegisterKeyNotify(InputEx,
			&hotkey,
			(EFI_KEY_NOTIFY_FUNCTION)&myNotify,
                        (VOID**)&notifyHandle);
	Print(L"RegisterKeyNotify=%r\n", Status);

	while( key.Key.UnicodeChar != 'q')
	{
		UINTN Index;
//		gBS->WaitForEvent(1, &(InputEx->WaitForKeyEx),  &Index);
		Status = uefi_call_wrapper(ST->BootServices->WaitForEvent, 3, 1, &InputEx->WaitForKeyEx, &Index);
		Print(L"Wait=%r\n", Status);
		if(Status)break;

	        Status = InputEx->ReadKeyStrokeEx(InputEx,
				&key);
		Print(L"(Scan %x Unicode%x)%x %x %llxh %r\n", key.Key.ScanCode, key.Key.UnicodeChar, key.KeyState.KeyShiftState, key.KeyState.KeyToggleState,key.Key, Status);
		if(key.Key.UnicodeChar == 'q')
			break;
	}
	Status = InputEx->UnregisterKeyNotify(InputEx, notifyHandle);
	return Status;

}

typedef VOID (*AccessFileInfo)(EFI_FILE_INFO* FileInfo);

EFI_STATUS 
GetFileIo( EFI_FILE_PROTOCOL** Root)
{
	EFI_STATUS  Status = 0;
    EFI_SIMPLE_FILE_SYSTEM_PROTOCOL *SimpleFileSystem;
    Status = gBS->LocateProtocol(
            &gEfiSimpleFileSystemProtocolGuid,
            NULL,
            (VOID**)&SimpleFileSystem
    );
    if (EFI_ERROR(Status)) {
     //Î´ÕÒµ½EFI_SIMPLE_FILE_SYSTEM_PROTOCOL
        return Status;
    }
    Status = SimpleFileSystem->OpenVolume(SimpleFileSystem, Root);
    return Status;
}


EFI_STATUS
ListDirectory(EFI_FILE_PROTOCOL* Directory, AccessFileInfo callbk)
{
        UINTN                BufferSize;
        UINTN                ReadSize;
        EFI_STATUS  Status = 0;
        EFI_FILE_INFO* FileInfo;

        BufferSize = sizeof(EFI_FILE_INFO) + sizeof(CHAR16) * 512;
        Status = gBS -> AllocatePool( EfiBootServicesCode, BufferSize, (VOID**)&FileInfo); 
        while(1){
            ReadSize = BufferSize;
            Status = Directory -> Read(Directory, &ReadSize, FileInfo); 
            if(Status == EFI_BUFFER_TOO_SMALL){
                BufferSize = ReadSize;
                Status = gBS -> FreePool( FileInfo);
                Status = gBS -> AllocatePool( EfiBootServicesCode, BufferSize, (VOID**)&FileInfo); 
                Status = Directory-> Read(Directory, &ReadSize, FileInfo);
            }

            if(ReadSize == 0) break;
            callbk(FileInfo);
        }
        Status = gBS -> FreePool( FileInfo);
        return 0;
}

VOID ListFileInfo(EFI_FILE_INFO* FileInfo)
{
    Print(L"Size : %d nFileSize:%d nPhysical Size:%d ",  FileInfo->Size, FileInfo->FileSize, FileInfo->PhysicalSize);
    Print(L"%s\n", FileInfo->FileName);
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

	EFI_FILE_HANDLE FileHandle;
        EFI_STATUS Status =GCCOpenFile( &FileHandle, L"ForthLib\\DATT.TXT", EFI_FILE_MODE_READ);
	
	Print(L" OpenFile = %llxh st=%x\n", FileHandle,Status);
	if(FileHandle)
	{
	UINT64  ReadSize = GCCFileSize(FileHandle);
	Print(L" ReadSize = %llxh\n", ReadSize);

	UINT16  *Buffer = AllocatePool(ReadSize);
	GCCReadFile(FileHandle,&ReadSize, Buffer);
//	uefi_call_wrapper(FileHandle->Read, 3, FileHandle, &ReadSize, Buffer);
	uefi_call_wrapper(ST->ConOut->OutputString, 2, ST->ConOut, Buffer);
	GCCCloseFile(FileHandle);
	}
//	pek();

//    EFI_FILE_PROTOCOL               *Root;
//    efi_status = GetFileIo(&Root);
//	Print(L"GetFileIo= %xh (%r)\n", efi_status, efi_status);
//    ListDirectory(Root, ListFileInfo);

//    EFI_FILE_PROTOCOL               *Mydir;
     EFI_FILE_HANDLE pMydir;
        Status =GCCOpenFile( &pMydir, L".", EFI_FILE_MODE_READ);
	Print(L" Mydir = %llxh st=%x\n", pMydir,Status);
    ListDirectory(pMydir, ListFileInfo);
	Status = GCCCloseFile(pMydir);
	Print(L" CloseMydir st=%x\n",Status);

        Status =GCCOpenFile( &pMydir, L"ForthLib", EFI_FILE_MODE_READ);
    ListDirectory(pMydir, ListFileInfo);
	Status = GCCCloseFile(pMydir);
	Print(L" CloseMydir st=%x\n",Status);

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
