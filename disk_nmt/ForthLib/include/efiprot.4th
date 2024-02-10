
\ File attributes
#define EFI_FILE_READ_ONLY      0x01
#define EFI_FILE_HIDDEN         0x02
#define EFI_FILE_SYSTEM         0x04
#define EFI_FILE_RESERVIED      0x08
#define EFI_FILE_DIRECTORY      0x10
#define EFI_FILE_ARCHIVE        0x20
#define EFI_FILE_VALID_ATTR     0x37

STRUCTURES{

  0
    *FIELD f.Revision
    *FIELD f.Open
    *FIELD f.Close
    *FIELD f.Delete
    *FIELD f.Read
    *FIELD f.Write
    *FIELD f.GetPosition
    *FIELD f.SetPosition
    *FIELD f.GetInfo
    *FIELD f.SetInfo
    *FIELD f.Flush
    *FIELD f.OpenEx
    *FIELD f.ReadEx
    *FIELD f.WriteEx
    *FIELD f.FlushEx
 DROP

\ typedef EFI_FILE_PROTOCOL EFI_FILE;

\ File information types

\ #define EFI_FILE_INFO_ID   \
\   { 0x9576e92, 0x6d3f, 0x11d2, {0x8e, 0x39, 0x0, 0xa0, 0xc9, 0x69, 0x72, 0x3b} }

 0
    *FIELD                  FI.Size
    *FIELD                  FI.FileSize
    *FIELD                  FI.PhysicalSize
    /EFI_TIME FIELD         FI.CreateTime
    /EFI_TIME FIELD         FI.LastAccessTime
    /EFI_TIME FIELD         FI.ModificationTime
    *FIELD                  FI.Attribute
   2 FIELD                  FI.FileName
 CONSTANT /EFI_FILE_INFO

}STRUCTURES

