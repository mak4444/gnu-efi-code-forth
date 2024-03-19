
#include "shim.h"

/*
 * Perform basic bounds checking of the intra-image pointers
 */
void *
ImageAddress (void *image, uint64_t size, uint64_t address)
{
	uintptr_t img_addr;

	/* ensure our local pointer isn't bigger than our size */
	if (address >= size)
		return NULL;

	/* Insure our math won't overflow */
	img_addr = (uintptr_t)image;
	if (checked_add(img_addr, address, &img_addr))
		return NULL;

	/* return the absolute pointer */
	return (void *)img_addr;
}

/*
 * Perform the actual relocation
 */
EFI_STATUS
relocate_coff (PE_COFF_LOADER_IMAGE_CONTEXT *context,
	       EFI_IMAGE_SECTION_HEADER *Section,
	       void *orig, void *data)
{
	EFI_IMAGE_BASE_RELOCATION *RelocBase, *RelocBaseEnd;
	UINT64 Adjust;
	UINT16 *Reloc, *RelocEnd;
	char *Fixup, *FixupBase;
	UINT16 *Fixup16;
	UINT32 *Fixup32;
	UINT64 *Fixup64;
	int size = context->ImageSize;
	void *ImageEnd = (char *)orig + size;
	int n = 0;

	/* Alright, so here's how this works:
	 *
	 * context->RelocDir gives us two things:
	 * - the VA the table of base relocation blocks are (maybe) to be
	 *   mapped at (RelocDir->VirtualAddress)
	 * - the virtual size (RelocDir->Size)
	 *
	 * The .reloc section (Section here) gives us some other things:
	 * - the name! kind of. (Section->Name)
	 * - the virtual size (Section->VirtualSize), which should be the same
	 *   as RelocDir->Size
	 * - the virtual address (Section->VirtualAddress)
	 * - the file section size (Section->SizeOfRawData), which is
	 *   a multiple of OptHdr->FileAlignment.  Only useful for image
	 *   validation, not really useful for iteration bounds.
	 * - the file address (Section->PointerToRawData)
	 * - a bunch of stuff we don't use that's 0 in our binaries usually
	 * - Flags (Section->Characteristics)
	 *
	 * and then the thing that's actually at the file address is an array
	 * of EFI_IMAGE_BASE_RELOCATION structs with some values packed behind
	 * them.  The SizeOfBlock field of this structure includes the
	 * structure itself, and adding it to that structure's address will
	 * yield the next entry in the array.
	 */
	RelocBase = ImageAddress(orig, size, Section->PointerToRawData);
	/* RelocBaseEnd here is the address of the first entry /past/ the
	 * table.  */
	RelocBaseEnd = ImageAddress(orig, size, Section->PointerToRawData +
						context->RelocDir->Size - 1);

	if (!RelocBase && !RelocBaseEnd)
		return EFI_SUCCESS;

	if (!RelocBase || !RelocBaseEnd) {
		Print(L"Reloc table overflows binary\n");
		return EFI_UNSUPPORTED;
	}

	Adjust = (UINTN)data - context->ImageAddress;

	if (Adjust == 0)
		return EFI_SUCCESS;

	while (RelocBase < RelocBaseEnd) {
		Reloc = (UINT16 *) ((char *) RelocBase + sizeof (EFI_IMAGE_BASE_RELOCATION));

		if (RelocBase->SizeOfBlock == 0) {
			Print(L"Reloc %d block size 0 is invalid\n", n);
			return EFI_UNSUPPORTED;
		} else if (RelocBase->SizeOfBlock > context->RelocDir->Size) {
			Print(L"Reloc %d block size %d greater than reloc dir"
					"size %d, which is invalid\n", n,
					RelocBase->SizeOfBlock,
					context->RelocDir->Size);
			return EFI_UNSUPPORTED;
		}

		RelocEnd = (UINT16 *) ((char *) RelocBase + RelocBase->SizeOfBlock);
		if ((void *)RelocEnd < orig || (void *)RelocEnd > ImageEnd) {
			Print(L"Reloc %d entry overflows binary\n", n);
			return EFI_UNSUPPORTED;
		}

		FixupBase = ImageAddress(data, size, RelocBase->VirtualAddress);
		if (!FixupBase) {
			Print(L"Reloc %d Invalid fixupbase\n", n);
			return EFI_UNSUPPORTED;
		}

		while (Reloc < RelocEnd) {
			Fixup = FixupBase + (*Reloc & 0xFFF);
			switch ((*Reloc) >> 12) {
			case EFI_IMAGE_REL_BASED_ABSOLUTE:
				break;

			case EFI_IMAGE_REL_BASED_HIGH:
				Fixup16   = (UINT16 *) Fixup;
				*Fixup16 = (UINT16) (*Fixup16 + ((UINT16) ((UINT32) Adjust >> 16)));
				break;

			case EFI_IMAGE_REL_BASED_LOW:
				Fixup16   = (UINT16 *) Fixup;
				*Fixup16  = (UINT16) (*Fixup16 + (UINT16) Adjust);
				break;

			case EFI_IMAGE_REL_BASED_HIGHLOW:
				Fixup32   = (UINT32 *) Fixup;
				*Fixup32  = *Fixup32 + (UINT32) Adjust;
				break;

			case EFI_IMAGE_REL_BASED_DIR64:
				Fixup64 = (UINT64 *) Fixup;
				*Fixup64 = *Fixup64 + (UINT64) Adjust;
				break;

			default:
				Print(L"Reloc %d Unknown relocation\n", n);
				return EFI_UNSUPPORTED;
			}
			Reloc += 1;
		}
		RelocBase = (EFI_IMAGE_BASE_RELOCATION *) RelocEnd;
		n++;
	}

	return EFI_SUCCESS;
}

EFI_STATUS
get_section_vma (UINTN section_num,
		 char *buffer, size_t bufsz ,
		 PE_COFF_LOADER_IMAGE_CONTEXT *context,
		 char **basep, size_t *sizep,
		 EFI_IMAGE_SECTION_HEADER **sectionp)
{
	return 0;
		Print(L"",section_num, buffer,bufsz,context, basep,sizep,
 sectionp);
}

EFI_STATUS
get_section_vma_by_name (char *name, size_t namesz,
			 char *buffer, size_t bufsz,
			 PE_COFF_LOADER_IMAGE_CONTEXT *context,
			 char **basep, size_t *sizep,
			 EFI_IMAGE_SECTION_HEADER **sectionp)
{
	return 0;
		Print(L"",name,  namesz,buffer,  bufsz,context, basep,sizep,
 sectionp);
}

/* here's a chart:
 *		i686	x86_64	aarch64
 *  64-on-64:	nyet	yes	yes
 *  64-on-32:	nyet	yes	nyet
 *  32-on-32:	yes	yes	no
 */
static int
allow_64_bit(void)
{
#if defined(__x86_64__) || defined(__aarch64__)
	return 1;
#elif defined(__i386__) || defined(__i686__)
	/* Right now blindly assuming the kernel will correctly detect this
	 * and /halt the system/ if you're not really on a 64-bit cpu */
	if (in_protocol)
		return 1;
	return 0;
#else /* assuming everything else is 32-bit... */
	return 0;
#endif
}

static int
allow_32_bit(void)
{
#if defined(__x86_64__)
#if defined(ALLOW_32BIT_KERNEL_ON_X64)
	if (in_protocol)
		return 1;
	return 0;
#else
	return 0;
#endif
#elif defined(__i386__) || defined(__i686__)
	return 1;
#elif defined(__aarch64__)
	return 0;
#else /* assuming everything else is 32-bit... */
	return 1;
#endif
}

static int
image_is_64_bit(EFI_IMAGE_OPTIONAL_HEADER_UNION *PEHdr)
{
	/* .Magic is the same offset in all cases */
	if (PEHdr->Pe32.OptionalHeader.Magic
			== EFI_IMAGE_NT_OPTIONAL_HDR64_MAGIC)
		return 1;
	return 0;
}

static const UINT16 machine_type =
#if defined(__x86_64__)
	IMAGE_FILE_MACHINE_X64;
#elif defined(__aarch64__)
	IMAGE_FILE_MACHINE_ARM64;
#elif defined(__arm__)
	IMAGE_FILE_MACHINE_ARMTHUMB_MIXED;
#elif defined(__i386__) || defined(__i486__) || defined(__i686__)
	IMAGE_FILE_MACHINE_I386;
#elif defined(__ia64__)
	IMAGE_FILE_MACHINE_IA64;
#else
#error this architecture is not supported by shim
#endif

static int
image_is_loadable(EFI_IMAGE_OPTIONAL_HEADER_UNION *PEHdr)
{
	/* If the machine type doesn't match the binary, bail, unless
	 * we're in an allowed 64-on-32 scenario */
	if (PEHdr->Pe32.FileHeader.Machine != machine_type) {
		if (!(machine_type == IMAGE_FILE_MACHINE_I386 &&
		      PEHdr->Pe32.FileHeader.Machine == IMAGE_FILE_MACHINE_X64 &&
		      allow_64_bit())) {
			return 0;
		}
	}

	/* If it's not a header type we recognize at all, bail */
	switch (PEHdr->Pe32Plus.OptionalHeader.Magic) {
	case EFI_IMAGE_NT_OPTIONAL_HDR64_MAGIC:
	case EFI_IMAGE_NT_OPTIONAL_HDR32_MAGIC:
		break;
	default:
		return 0;
	}

	/* and now just check for general 64-vs-32 compatibility */
	if (image_is_64_bit(PEHdr)) {
		if (allow_64_bit())
			return 1;
	} else {
		if (allow_32_bit())
			return 1;
	}
	return 0;
}

/*
 * Read the binary header and grab appropriate information from it
 */
EFI_STATUS
read_header(void *data, unsigned int datasize,
	    PE_COFF_LOADER_IMAGE_CONTEXT *context)
{
	EFI_IMAGE_DOS_HEADER *DosHdr = data;
	EFI_IMAGE_OPTIONAL_HEADER_UNION *PEHdr = data;
	unsigned long HeaderWithoutDataDir, SectionHeaderOffset, OptHeaderSize;
	unsigned long FileAlignment = 0;
	size_t dos_sz = 0;
	size_t tmpsz0, tmpsz1;

	if (datasize < sizeof (*DosHdr)) {
		Print(L"datasize Invalid image\n");
		return EFI_UNSUPPORTED;
	}

	if (DosHdr->e_magic == EFI_IMAGE_DOS_SIGNATURE) {
		if (DosHdr->e_lfanew < sizeof (*DosHdr) ||
		    DosHdr->e_lfanew > datasize - 4) {
			Print(L"lfanew Invalid image\n");
			return EFI_UNSUPPORTED;
		}

		dos_sz = DosHdr->e_lfanew;
		PEHdr = (EFI_IMAGE_OPTIONAL_HEADER_UNION *)((char *)data + DosHdr->e_lfanew);
	}

	if (datasize - dos_sz < sizeof (PEHdr->Pe32)) {
		Print(L"Pe32 Invalid image\n");
		return EFI_UNSUPPORTED;
	}

	if (image_is_64_bit(PEHdr) &&
	    (datasize - dos_sz < sizeof (PEHdr->Pe32Plus))) {
		Print(L"Pe32Plus Invalid image\n");
		return EFI_UNSUPPORTED;
	}

	if (!image_is_loadable(PEHdr)) {
		Print(L"Platform does not support this image\n");
		return EFI_UNSUPPORTED;
	}

	if (image_is_64_bit(PEHdr)) {
		context->NumberOfRvaAndSizes = PEHdr->Pe32Plus.OptionalHeader.NumberOfRvaAndSizes;
		context->SizeOfHeaders = PEHdr->Pe32Plus.OptionalHeader.SizeOfHeaders;
		context->ImageSize = PEHdr->Pe32Plus.OptionalHeader.SizeOfImage;
		context->SectionAlignment = PEHdr->Pe32Plus.OptionalHeader.SectionAlignment;
		FileAlignment = PEHdr->Pe32Plus.OptionalHeader.FileAlignment;
		OptHeaderSize = sizeof(EFI_IMAGE_OPTIONAL_HEADER64);
	} else {
		context->NumberOfRvaAndSizes = PEHdr->Pe32.OptionalHeader.NumberOfRvaAndSizes;
		context->SizeOfHeaders = PEHdr->Pe32.OptionalHeader.SizeOfHeaders;
		context->ImageSize = (UINT64)PEHdr->Pe32.OptionalHeader.SizeOfImage;
		context->SectionAlignment = PEHdr->Pe32.OptionalHeader.SectionAlignment;
		FileAlignment = PEHdr->Pe32.OptionalHeader.FileAlignment;
		OptHeaderSize = sizeof(EFI_IMAGE_OPTIONAL_HEADER32);
	}

	if (FileAlignment % 2 != 0) {
		Print(L"File Alignment is invalid (%d)\n", FileAlignment);
		return EFI_UNSUPPORTED;
	}
	if (FileAlignment == 0)
		FileAlignment = 0x200;
	if (context->SectionAlignment == 0)
		context->SectionAlignment = PAGE_SIZE;
	if (context->SectionAlignment < FileAlignment)
		context->SectionAlignment = FileAlignment;

	context->NumberOfSections = PEHdr->Pe32.FileHeader.NumberOfSections;

	if (EFI_IMAGE_NUMBER_OF_DIRECTORY_ENTRIES < context->NumberOfRvaAndSizes) {
		Print(L"Image header too large\n");
		return EFI_UNSUPPORTED;
	}

	if (checked_mul(sizeof(EFI_IMAGE_DATA_DIRECTORY), EFI_IMAGE_NUMBER_OF_DIRECTORY_ENTRIES, &tmpsz0) ||
	    checked_sub(OptHeaderSize, tmpsz0, &HeaderWithoutDataDir) ||
	    checked_sub((size_t)PEHdr->Pe32.FileHeader.SizeOfOptionalHeader, HeaderWithoutDataDir, &tmpsz0) ||
	    checked_mul((size_t)context->NumberOfRvaAndSizes, sizeof (EFI_IMAGE_DATA_DIRECTORY), &tmpsz1) ||
	    (tmpsz0 != tmpsz1)) {
		Print(L"Image header overflows data directory\n");
		return EFI_UNSUPPORTED;
	}

	if (checked_add((size_t)DosHdr->e_lfanew, sizeof(UINT32), &tmpsz0) ||
	    checked_add(tmpsz0, sizeof(EFI_IMAGE_FILE_HEADER), &tmpsz0) ||
	    checked_add(tmpsz0, PEHdr->Pe32.FileHeader.SizeOfOptionalHeader, &SectionHeaderOffset)) {
		Print(L"Image sections overflow image size\n");
		return EFI_UNSUPPORTED;
	}

	if (checked_sub((size_t)context->ImageSize, SectionHeaderOffset, &tmpsz0) ||
	    (tmpsz0 / EFI_IMAGE_SIZEOF_SECTION_HEADER <= context->NumberOfSections)) {
		Print(L"Image sections overflow image size\n");
		return EFI_UNSUPPORTED;
	}

	if (checked_sub((size_t)context->SizeOfHeaders, SectionHeaderOffset, &tmpsz0) ||
	    (tmpsz0 / EFI_IMAGE_SIZEOF_SECTION_HEADER < (UINT32)context->NumberOfSections)) {
		Print(L"Image sections overflow section headers\n");
		return EFI_UNSUPPORTED;
	}

	if (checked_mul((size_t)context->NumberOfSections, sizeof(EFI_IMAGE_SECTION_HEADER), &tmpsz0) ||
	    checked_add(tmpsz0, SectionHeaderOffset, &tmpsz0) ||
	    (tmpsz0 > datasize)) {
		Print(L"Image sections overflow section headers\n");
		return EFI_UNSUPPORTED;
	}

	if (checked_sub((size_t)(uintptr_t)PEHdr, (size_t)(uintptr_t)data, &tmpsz0) ||
	    checked_add(tmpsz0, sizeof(EFI_IMAGE_OPTIONAL_HEADER_UNION), &tmpsz0) ||
	    (tmpsz0 > datasize)) {
		Print(L"Invalid image\n");
		return EFI_UNSUPPORTED;
	}

	if (PEHdr->Te.Signature != EFI_IMAGE_NT_SIGNATURE) {
		Print(L"Unsupported image type\n");
		return EFI_UNSUPPORTED;
	}

	if (PEHdr->Pe32.FileHeader.Characteristics & EFI_IMAGE_FILE_RELOCS_STRIPPED) {
		Print(L"Unsupported image - Relocations have been stripped\n");
		return EFI_UNSUPPORTED;
	}

	context->PEHdr = PEHdr;

	if (image_is_64_bit(PEHdr)) {
		context->ImageAddress = PEHdr->Pe32Plus.OptionalHeader.ImageBase;
		context->EntryPoint = PEHdr->Pe32Plus.OptionalHeader.AddressOfEntryPoint;
		context->RelocDir = &PEHdr->Pe32Plus.OptionalHeader.DataDirectory[EFI_IMAGE_DIRECTORY_ENTRY_BASERELOC];
		context->SecDir = &PEHdr->Pe32Plus.OptionalHeader.DataDirectory[EFI_IMAGE_DIRECTORY_ENTRY_SECURITY];
	} else {
		context->ImageAddress = PEHdr->Pe32.OptionalHeader.ImageBase;
		context->EntryPoint = PEHdr->Pe32.OptionalHeader.AddressOfEntryPoint;
		context->RelocDir = &PEHdr->Pe32.OptionalHeader.DataDirectory[EFI_IMAGE_DIRECTORY_ENTRY_BASERELOC];
		context->SecDir = &PEHdr->Pe32.OptionalHeader.DataDirectory[EFI_IMAGE_DIRECTORY_ENTRY_SECURITY];
	}


	if (checked_add((size_t)(uintptr_t)PEHdr, PEHdr->Pe32.FileHeader.SizeOfOptionalHeader, &tmpsz0) ||
	    checked_add(tmpsz0, sizeof(UINT32), &tmpsz0) ||
	    checked_add(tmpsz0, sizeof(EFI_IMAGE_FILE_HEADER), &tmpsz0)) {
		Print(L"checked_add Invalid image\n");
		return EFI_UNSUPPORTED;
	}
	context->FirstSection = (EFI_IMAGE_SECTION_HEADER *)(uintptr_t)tmpsz0;
	if ((uint64_t)(uintptr_t)(context->FirstSection)
	    > (uint64_t)(uintptr_t)data + datasize) {
		Print(L"FirstSec Invalid image\n");
		return EFI_UNSUPPORTED;
	}

	if (context->ImageSize < context->SizeOfHeaders) {
		Print(L"FirstSection Invalid image\n");
		return EFI_UNSUPPORTED;
	}

	if (checked_sub((size_t)(uintptr_t)context->SecDir, (size_t)(uintptr_t)data, &tmpsz0) ||
	    (tmpsz0 > datasize - sizeof(EFI_IMAGE_DATA_DIRECTORY))) {
		Print(L"checked_sub Invalid image\n");
		return EFI_UNSUPPORTED;
	}

	if (context->SecDir->VirtualAddress > datasize ||
	    (context->SecDir->VirtualAddress == datasize &&
	     context->SecDir->Size > 0)) {
		Print(L"Malformed security header\n");
		return EFI_INVALID_PARAMETER;
	}
	return EFI_SUCCESS;
}

// vim:fenc=utf-8:tw=75:noet
