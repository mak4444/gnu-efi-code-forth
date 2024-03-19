// SPDX-License-Identifier: BSD-2-Clause-Patent
#include <efi.h>
#include <efilib.h>
#include <eficonex.h>
#include <simple_file.h>
#include "lib.h"
#include <guid.h>

#define DEFAULT_LOADER L"\\bltgrid.efi"

extern UINT8 in_protocol;
extern CHAR16 *second_stage;
extern void *load_options;
extern UINT32 load_options_size;

//
// Directory Entries
//
#define EFI_IMAGE_DIRECTORY_ENTRY_EXPORT      0
#define EFI_IMAGE_DIRECTORY_ENTRY_IMPORT      1
#define EFI_IMAGE_DIRECTORY_ENTRY_RESOURCE    2
#define EFI_IMAGE_DIRECTORY_ENTRY_EXCEPTION   3
#define EFI_IMAGE_DIRECTORY_ENTRY_SECURITY    4
#define EFI_IMAGE_DIRECTORY_ENTRY_BASERELOC   5
#define EFI_IMAGE_DIRECTORY_ENTRY_DEBUG       6
#define EFI_IMAGE_DIRECTORY_ENTRY_COPYRIGHT   7
#define EFI_IMAGE_DIRECTORY_ENTRY_GLOBALPTR   8
#define EFI_IMAGE_DIRECTORY_ENTRY_TLS         9
#define EFI_IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG 10

#define EFI_IMAGE_NUMBER_OF_DIRECTORY_ENTRIES 16

//
// Based relocation types.
//
#define EFI_IMAGE_REL_BASED_ABSOLUTE        0
#define EFI_IMAGE_REL_BASED_HIGH            1
#define EFI_IMAGE_REL_BASED_LOW             2
#define EFI_IMAGE_REL_BASED_HIGHLOW         3
#define EFI_IMAGE_REL_BASED_HIGHADJ         4
#define EFI_IMAGE_REL_BASED_MIPS_JMPADDR    5
#define EFI_IMAGE_REL_BASED_ARM_MOV32A      5
#define EFI_IMAGE_REL_BASED_ARM_MOV32T      7
#define EFI_IMAGE_REL_BASED_IA64_IMM64      9
#define EFI_IMAGE_REL_BASED_MIPS_JMPADDR16  9
#define EFI_IMAGE_REL_BASED_DIR64           10


//
// PE32+ Machine type for EFI images
//
#define IMAGE_FILE_MACHINE_I386            0x014c
#define IMAGE_FILE_MACHINE_IA64            0x0200
#define IMAGE_FILE_MACHINE_EBC             0x0EBC
#define IMAGE_FILE_MACHINE_X64             0x8664
#define IMAGE_FILE_MACHINE_ARMTHUMB_MIXED  0x01c2
#define IMAGE_FILE_MACHINE_ARM64	   0xaa64

///
/// @attention
/// EFI_IMAGE_NT_OPTIONAL_HDR32_MAGIC means PE32 and 
/// EFI_IMAGE_OPTIONAL_HEADER32 must be used. The data structures only vary
/// after NT additional fields.
///
#define EFI_IMAGE_NT_OPTIONAL_HDR32_MAGIC 0x10b


///
/// @attention
/// EFI_IMAGE_NT_OPTIONAL_HDR64_MAGIC means PE32+ and 
/// EFI_IMAGE_OPTIONAL_HEADER64 must be used. The data structures only vary
/// after NT additional fields.
///
#define EFI_IMAGE_NT_OPTIONAL_HDR64_MAGIC 0x20b

EFI_STATUS init_grub(EFI_HANDLE image_handle);

extern UINT32 load_options_size;


extern int loader_is_participating;

///
/// Based relocation format.
///
typedef struct {
  UINT32  VirtualAddress;
  UINT32  SizeOfBlock;
} EFI_IMAGE_BASE_RELOCATION;

EFI_STATUS
handle_image (void *data, unsigned int datasize,
	      EFI_LOADED_IMAGE *li,
	      EFI_IMAGE_ENTRY_POINT *entry_point,
	      EFI_PHYSICAL_ADDRESS *alloc_address,
	      UINTN *alloc_pages);


EFI_STATUS generate_path_from_image_path(EFI_LOADED_IMAGE *li,
					 CHAR16 *ImagePath,
					 CHAR16 **PathName);


#define EFI_IMAGE_SIZEOF_SHORT_NAME 8
#define EFI_IMAGE_NUMBER_OF_DIRECTORY_ENTRIES 16

///
/// Header Data Directories.
///
typedef struct {
  UINT32  VirtualAddress;
  UINT32  Size;
} EFI_IMAGE_DATA_DIRECTORY;

///
/// Section Table. This table immediately follows the optional header.
///
typedef struct {
  UINT8 Name[EFI_IMAGE_SIZEOF_SHORT_NAME];
  union {
    UINT32  PhysicalAddress;
    UINT32  VirtualSize;
  } Misc;
  UINT32  VirtualAddress;
  UINT32  SizeOfRawData;
  UINT32  PointerToRawData;
  UINT32  PointerToRelocations;
  UINT32  PointerToLinenumbers;
  UINT16  NumberOfRelocations;
  UINT16  NumberOfLinenumbers;
  UINT32  Characteristics;
} EFI_IMAGE_SECTION_HEADER;

///
/// COFF File Header (Object and Image).
///
typedef struct {
  UINT16  Machine;
  UINT16  NumberOfSections;
  UINT32  TimeDateStamp;
  UINT32  PointerToSymbolTable;
  UINT32  NumberOfSymbols;
  UINT16  SizeOfOptionalHeader;
  UINT16  Characteristics;
} EFI_IMAGE_FILE_HEADER;

///
/// Optional Header Standard Fields for PE32.
///
typedef struct {
  ///
  /// Standard fields.
  ///
  UINT16                    Magic;
  UINT8                     MajorLinkerVersion;
  UINT8                     MinorLinkerVersion;
  UINT32                    SizeOfCode;
  UINT32                    SizeOfInitializedData;
  UINT32                    SizeOfUninitializedData;
  UINT32                    AddressOfEntryPoint;
  UINT32                    BaseOfCode;
  UINT32                    BaseOfData;  ///< PE32 contains this additional field, which is absent in PE32+.
  ///
  /// Optional Header Windows-Specific Fields.
  ///
  UINT32                    ImageBase;
  UINT32                    SectionAlignment;
  UINT32                    FileAlignment;
  UINT16                    MajorOperatingSystemVersion;
  UINT16                    MinorOperatingSystemVersion;
  UINT16                    MajorImageVersion;
  UINT16                    MinorImageVersion;
  UINT16                    MajorSubsystemVersion;
  UINT16                    MinorSubsystemVersion;
  UINT32                    Win32VersionValue;
  UINT32                    SizeOfImage;
  UINT32                    SizeOfHeaders;
  UINT32                    CheckSum;
  UINT16                    Subsystem;
  UINT16                    DllCharacteristics;
  UINT32                    SizeOfStackReserve;
  UINT32                    SizeOfStackCommit;
  UINT32                    SizeOfHeapReserve;
  UINT32                    SizeOfHeapCommit;
  UINT32                    LoaderFlags;
  UINT32                    NumberOfRvaAndSizes;
  EFI_IMAGE_DATA_DIRECTORY  DataDirectory[EFI_IMAGE_NUMBER_OF_DIRECTORY_ENTRIES];
} EFI_IMAGE_OPTIONAL_HEADER32;

typedef struct {
  UINT32                      Signature;
  EFI_IMAGE_FILE_HEADER       FileHeader;
  EFI_IMAGE_OPTIONAL_HEADER32 OptionalHeader;
} EFI_IMAGE_NT_HEADERS32;


///
/// Optional Header Standard Fields for PE32+.
///
typedef struct {
  ///
  /// Standard fields.
  ///
  UINT16                    Magic;
  UINT8                     MajorLinkerVersion;
  UINT8                     MinorLinkerVersion;
  UINT32                    SizeOfCode;
  UINT32                    SizeOfInitializedData;
  UINT32                    SizeOfUninitializedData;
  UINT32                    AddressOfEntryPoint;
  UINT32                    BaseOfCode;
  ///
  /// Optional Header Windows-Specific Fields.
  ///
  UINT64                    ImageBase;
  UINT32                    SectionAlignment;
  UINT32                    FileAlignment;
  UINT16                    MajorOperatingSystemVersion;
  UINT16                    MinorOperatingSystemVersion;
  UINT16                    MajorImageVersion;
  UINT16                    MinorImageVersion;
  UINT16                    MajorSubsystemVersion;
  UINT16                    MinorSubsystemVersion;
  UINT32                    Win32VersionValue;
  UINT32                    SizeOfImage;
  UINT32                    SizeOfHeaders;
  UINT32                    CheckSum;
  UINT16                    Subsystem;
  UINT16                    DllCharacteristics;
  UINT64                    SizeOfStackReserve;
  UINT64                    SizeOfStackCommit;
  UINT64                    SizeOfHeapReserve;
  UINT64                    SizeOfHeapCommit;
  UINT32                    LoaderFlags;
  UINT32                    NumberOfRvaAndSizes;
  EFI_IMAGE_DATA_DIRECTORY  DataDirectory[EFI_IMAGE_NUMBER_OF_DIRECTORY_ENTRIES];
} EFI_IMAGE_OPTIONAL_HEADER64;

typedef struct {
  UINT32                      Signature;
  EFI_IMAGE_FILE_HEADER       FileHeader;
  EFI_IMAGE_OPTIONAL_HEADER64 OptionalHeader;
} EFI_IMAGE_NT_HEADERS64;


///
/// Header format for TE images, defined in the PI Specification, 1.0.
///
typedef struct {
  UINT16                    Signature;            ///< The signature for TE format = "VZ".
  UINT16                    Machine;              ///< From the original file header.
  UINT8                     NumberOfSections;     ///< From the original file header.
  UINT8                     Subsystem;            ///< From original optional header.
  UINT16                    StrippedSize;         ///< Number of bytes we removed from the header.
  UINT32                    AddressOfEntryPoint;  ///< Offset to entry point -- from original optional header.
  UINT32                    BaseOfCode;           ///< From original image -- required for ITP debug.
  UINT64                    ImageBase;            ///< From original file header.
  EFI_IMAGE_DATA_DIRECTORY  DataDirectory[2];     ///< Only base relocation and debug directory.
} EFI_TE_IMAGE_HEADER;

typedef union {
  EFI_IMAGE_NT_HEADERS32   Pe32;
  EFI_IMAGE_NT_HEADERS64   Pe32Plus;
  EFI_TE_IMAGE_HEADER      Te;
} EFI_IMAGE_OPTIONAL_HEADER_UNION;


typedef struct {
	UINT64 ImageAddress;
	UINT64 ImageSize;
	UINT64 EntryPoint;
	UINTN SizeOfHeaders;
	UINT16 ImageType;
	UINT16 NumberOfSections;
	UINT32 SectionAlignment;
	EFI_IMAGE_SECTION_HEADER *FirstSection;
	EFI_IMAGE_DATA_DIRECTORY *RelocDir;
	EFI_IMAGE_DATA_DIRECTORY *SecDir;
	UINT64 NumberOfRvaAndSizes;
	EFI_IMAGE_OPTIONAL_HEADER_UNION *PEHdr;
} PE_COFF_LOADER_IMAGE_CONTEXT;



/*
 * PE 9.3 says both IMAGE_SCN_MEM_PURGEABLE and IMAGE_SCN_MEM_16BIT are
 * 0x00020000, but I think it's wrong. --pjones
 */
#define EFI_IMAGE_SCN_MEM_PURGEABLE                0x00010000 // "Reserved for future use."
#define EFI_IMAGE_SCN_MEM_16BIT                    0x00020000 // "Reserved for future use."
#define EFI_IMAGE_SCN_MEM_LOCKED                   0x00040000 // "Reserved for future use."
#define EFI_IMAGE_SCN_MEM_PRELOAD                  0x00080000 // "Reserved for future use."
#define EFI_IMAGE_SCN_ALIGN_1BYTES                 0x00100000
#define EFI_IMAGE_SCN_ALIGN_2BYTES                 0x00200000
#define EFI_IMAGE_SCN_ALIGN_4BYTES                 0x00300000
#define EFI_IMAGE_SCN_ALIGN_8BYTES                 0x00400000
#define EFI_IMAGE_SCN_ALIGN_16BYTES                0x00500000
#define EFI_IMAGE_SCN_ALIGN_32BYTES                0x00600000
#define EFI_IMAGE_SCN_ALIGN_64BYTES                0x00700000
#define EFI_IMAGE_SCN_ALIGN_128BYTES               0x00800000
#define EFI_IMAGE_SCN_ALIGN_256BYTES               0x00900000
#define EFI_IMAGE_SCN_ALIGN_512BYTES               0x00a00000
#define EFI_IMAGE_SCN_ALIGN_1024BYTES              0x00b00000
#define EFI_IMAGE_SCN_ALIGN_2048BYTES              0x00c00000
#define EFI_IMAGE_SCN_ALIGN_4096BYTES              0x00d00000
#define EFI_IMAGE_SCN_ALIGN_8192BYTES              0x00e00000
#define EFI_IMAGE_SCN_LNK_NRELOC_OVFL              0x01000000
#define EFI_IMAGE_SCN_MEM_DISCARDABLE              0x02000000
#define EFI_IMAGE_SCN_MEM_NOT_CACHED               0x04000000
#define EFI_IMAGE_SCN_MEM_NOT_PAGED                0x08000000
#define EFI_IMAGE_SCN_MEM_SHARED                   0x10000000
#define EFI_IMAGE_SCN_MEM_EXECUTE                  0x20000000
#define EFI_IMAGE_SCN_MEM_READ                     0x40000000
#define EFI_IMAGE_SCN_MEM_WRITE                    0x80000000

#define EFI_IMAGE_SIZEOF_SECTION_HEADER       40


#define SIGNATURE_16(A, B) \
	((UINT16)(((UINT16)(A)) | (((UINT16)(B)) << ((UINT16)8))))
#define SIGNATURE_32(A, B, C, D)                 \
	((UINT32)(((UINT32)SIGNATURE_16(A, B)) | \
	          (((UINT32)SIGNATURE_16(C, D)) << (UINT32)16)))
#define SIGNATURE_64(A, B, C, D, E, F, G, H)         \
	((UINT64)((UINT64)SIGNATURE_32(A, B, C, D) | \
	          ((UINT64)(SIGNATURE_32(E, F, G, H)) << (UINT64)32)))

//
// EXE file formats
//
#define EFI_IMAGE_DOS_SIGNATURE     SIGNATURE_16('M', 'Z')
#define EFI_IMAGE_OS2_SIGNATURE     SIGNATURE_16('N', 'E')
#define EFI_IMAGE_OS2_SIGNATURE_LE  SIGNATURE_16('L', 'E')
#define EFI_IMAGE_NT_SIGNATURE      SIGNATURE_32('P', 'E', '\0', '\0')

///
/// PE images can start with an optional DOS header, so if an image is run
/// under DOS it can print an error message.
///
typedef struct {
  UINT16  e_magic;    ///< Magic number.
  UINT16  e_cblp;     ///< Bytes on last page of file.
  UINT16  e_cp;       ///< Pages in file.
  UINT16  e_crlc;     ///< Relocations.
  UINT16  e_cparhdr;  ///< Size of header in paragraphs.
  UINT16  e_minalloc; ///< Minimum extra paragraphs needed.
  UINT16  e_maxalloc; ///< Maximum extra paragraphs needed.
  UINT16  e_ss;       ///< Initial (relative) SS value.
  UINT16  e_sp;       ///< Initial SP value.
  UINT16  e_csum;     ///< Checksum.
  UINT16  e_ip;       ///< Initial IP value.
  UINT16  e_cs;       ///< Initial (relative) CS value.
  UINT16  e_lfarlc;   ///< File address of relocation table.
  UINT16  e_ovno;     ///< Overlay number.
  UINT16  e_res[4];   ///< Reserved words.
  UINT16  e_oemid;    ///< OEM identifier (for e_oeminfo).
  UINT16  e_oeminfo;  ///< OEM information; e_oemid specific.
  UINT16  e_res2[10]; ///< Reserved words.
  UINT32  e_lfanew;   ///< File address of new exe header.
} EFI_IMAGE_DOS_HEADER;

#define checked_add(addend0, addend1, sum) \
	__builtin_add_overflow(addend0, addend1, sum)
#define checked_sub(minuend, subtrahend, difference) \
	__builtin_sub_overflow(minuend, subtrahend, difference)
#define checked_mul(factor0, factor1, product) \
	__builtin_mul_overflow(factor0, factor1, product)

//
// Characteristics
//
#define EFI_IMAGE_FILE_RELOCS_STRIPPED      (1 << 0)     ///< 0x0001  Relocation info stripped from file.
#define EFI_IMAGE_FILE_EXECUTABLE_IMAGE     (1 << 1)     ///< 0x0002  File is executable  (i.e. no unresolved externel references).
#define EFI_IMAGE_FILE_LINE_NUMS_STRIPPED   (1 << 2)     ///< 0x0004  Line nunbers stripped from file.
#define EFI_IMAGE_FILE_LOCAL_SYMS_STRIPPED  (1 << 3)     ///< 0x0008  Local symbols stripped from file.
#define EFI_IMAGE_FILE_BYTES_REVERSED_LO    (1 << 7)     ///< 0x0080  Bytes of machine word are reversed.
#define EFI_IMAGE_FILE_32BIT_MACHINE        (1 << 8)     ///< 0x0100  32 bit word machine.
#define EFI_IMAGE_FILE_DEBUG_STRIPPED       (1 << 9)     ///< 0x0200  Debugging info stripped from file in .DBG file.
#define EFI_IMAGE_FILE_SYSTEM               (1 << 12)    ///< 0x1000  System File.
#define EFI_IMAGE_FILE_DLL                  (1 << 13)    ///< 0x2000  File is a DLL.
#define EFI_IMAGE_FILE_BYTES_REVERSED_HI    (1 << 15)    ///< 0x8000  Bytes of machine word are reversed.

void *
ImageAddress (void *image, uint64_t size, uint64_t address);
EFI_STATUS
read_header(void *data, unsigned int datasize,
	    PE_COFF_LOADER_IMAGE_CONTEXT *context);

#define ALIGN_VALUE(Value, Alignment) ((Value) + (((Alignment) - (Value)) & ((Alignment) - 1)))

static inline INTN
__attribute__((unused))
StrnCaseCmp(CHAR16 *s0, CHAR16 *s1, int n)
{
	CHAR16 c0, c1;
	int x = 0;
	while (n > x++) {
		if (*s0 == L'\0' || *s1 == L'\0')
			return *s1 - *s0;
		c0 = (*s0 >= L'a' && *s0 <= L'z') ? *s0 - 32 : *s0;
		c1 = (*s1 >= L'a' && *s1 <= L'z') ? *s1 - 32 : *s1;
		if (c0 != c1)
			return c1 - c0;
		s0++;
		s1++;
	}
	return 0;
}

EFI_STATUS start_efi(void *data,int datasize);


