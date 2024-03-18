
#include "shim.h"

/*
 * Calculate the SHA1 and SHA256 hashes of a binary
 */

EFI_STATUS
generate_hash(char *data, unsigned int datasize,
	      PE_COFF_LOADER_IMAGE_CONTEXT *context, UINT8 *sha256hash,
	      UINT8 *sha1hash)
{ return 0;
		Print(L"", data, datasize,context, sha256hash,sha1hash);
}

EFI_STATUS
verify_sbat_section(char *SBATBase, size_t SBATSize)
{ return 0;
		Print(L"", SBATBase, SBATSize);
}

/*
 * Once the image has been loaded it needs to be validated and relocated
 */
EFI_STATUS
handle_image (void *data, unsigned int datasize,
	      EFI_LOADED_IMAGE *li,
	      EFI_IMAGE_ENTRY_POINT *entry_point,
	      EFI_PHYSICAL_ADDRESS *alloc_address,
	      UINTN *alloc_pages)
{
	EFI_STATUS efi_status;
	char *buffer;
	int i;
	EFI_IMAGE_SECTION_HEADER *Section;
	char *base, *end;
	UINT32 size;
	PE_COFF_LOADER_IMAGE_CONTEXT context;
	unsigned int alignment, alloc_size;
	int found_entry_point = 0;
//	UINT8 sha1hash[SHA1_DIGEST_SIZE];
//	UINT8 sha256hash[SHA256_DIGEST_SIZE];

	/*
	 * The binary header contains relevant context and section pointers
	 */
	efi_status = read_header(data, datasize, &context);
	if (EFI_ERROR(efi_status)) {
		Print(L"Failed to read header: %r\n", efi_status);
		return efi_status;
	}


	/*
	 * Calculate the hash for the TPM measurement.
	 * XXX: We're computing these twice in secure boot mode when the
	 *  buffers already contain the previously computed hashes. Also,
	 *  this is only useful for the TPM1.2 case. We should try to fix
	 *  this in a follow-up.
	 */

	Print(L"",li);

	/* The spec says, uselessly, of SectionAlignment:
	 * =====
	 * The alignment (in bytes) of sections when they are loaded into
	 * memory. It must be greater than or equal to FileAlignment. The
	 * default is the page size for the architecture.
	 * =====
	 * Which doesn't tell you whose responsibility it is to enforce the
	 * "default", or when.  It implies that the value in the field must
	 * be > FileAlignment (also poorly defined), but it appears visual
	 * studio will happily write 512 for FileAlignment (its default) and
	 * 0 for SectionAlignment, intending to imply PAGE_SIZE.
	 *
	 * We only support one page size, so if it's zero, nerf it to 4096.
	 */
	alignment = context.SectionAlignment;
	if (!alignment)
		alignment = 4096;

	alloc_size = ALIGN_VALUE(context.ImageSize + context.SectionAlignment,
				 PAGE_SIZE);
	*alloc_pages = alloc_size / PAGE_SIZE;

	efi_status = BS->AllocatePages(AllocateAnyPages, EfiLoaderCode,
				       *alloc_pages, alloc_address);
	if (EFI_ERROR(efi_status)) {
		Print(L"Failed to allocate image buffer\n");
		return EFI_OUT_OF_RESOURCES;
	}

	buffer = (void *)ALIGN_VALUE((unsigned long)*alloc_address, alignment);
	Print(L"Loading 0x%llx bytes at 0x%llx\n",
	       (unsigned long long)context.ImageSize,
	       (unsigned long long)(uintptr_t)buffer);

	CopyMem(buffer, data, context.SizeOfHeaders);

	/* Flush the instruction cache for the region holding the image */
//	cache_invalidate(buffer, buffer + context.ImageSize);

	*entry_point = ImageAddress(buffer, context.ImageSize, context.EntryPoint);
	if (!*entry_point) {
		Print(L"Entry point is invalid\n");
		BS->FreePages(*alloc_address, *alloc_pages);
		return EFI_UNSUPPORTED;
	}

	char *RelocBase, *RelocBaseEnd;
	/*
	 * These are relative virtual addresses, so we have to check them
	 * against the image size, not the data size.
	 */
	RelocBase = ImageAddress(buffer, context.ImageSize,
				 context.RelocDir->VirtualAddress);
	/*
	 * RelocBaseEnd here is the address of the last byte of the table
	 */
	RelocBaseEnd = ImageAddress(buffer, context.ImageSize,
				    context.RelocDir->VirtualAddress +
				    context.RelocDir->Size - 1);

	EFI_IMAGE_SECTION_HEADER *RelocSection = NULL;

	/*
	 * Copy the executable's sections to their desired offsets
	 */
	Section = context.FirstSection;
	for (i = 0; i < context.NumberOfSections; i++, Section++) {
		/* Don't try to copy discardable sections with zero size */
		if ((Section->Characteristics & EFI_IMAGE_SCN_MEM_DISCARDABLE) &&
		    !Section->Misc.VirtualSize)
			continue;

		/*
		 * Skip sections that aren't marked readable.
		 */
		if (!(Section->Characteristics & EFI_IMAGE_SCN_MEM_READ))
			continue;

		base = ImageAddress (buffer, context.ImageSize,
				     Section->VirtualAddress);
		end = ImageAddress (buffer, context.ImageSize,
				    Section->VirtualAddress
				     + Section->Misc.VirtualSize - 1);

		if (end < base) {
			Print(L"Section %d has negative size\n", i);
			BS->FreePages(*alloc_address, *alloc_pages);
			return EFI_UNSUPPORTED;
		}

		if (Section->VirtualAddress <= context.EntryPoint &&
		    (Section->VirtualAddress + Section->Misc.VirtualSize - 1)
		    > context.EntryPoint)
			found_entry_point++;

		/* We do want to process .reloc, but it's often marked
		 * discardable, so we don't want to memcpy it. */
		if (CompareMem(Section->Name, ".reloc\0\0", 8) == 0) {
			if (RelocSection) {
				Print(L"Image has multiple relocation sections\n");
				BS->FreePages(*alloc_address, *alloc_pages);
				return EFI_UNSUPPORTED;
			}
			/* If it has nonzero sizes, and our bounds check
			 * made sense, and the VA and size match RelocDir's
			 * versions, then we believe in this section table. */
			if (Section->SizeOfRawData &&
					Section->Misc.VirtualSize &&
					base && end &&
					RelocBase == base &&
					RelocBaseEnd <= end) {
				RelocSection = Section;
			} else {
				Print(L"Relocation section is invalid \n");
				BS->FreePages(*alloc_address, *alloc_pages);
				return EFI_UNSUPPORTED;
			}
		}

		if (Section->Characteristics & EFI_IMAGE_SCN_MEM_DISCARDABLE) {
			continue;
		}

		if (!base) {
			Print(L"Section %d has invalid base address\n", i);
			BS->FreePages(*alloc_address, *alloc_pages);
			return EFI_UNSUPPORTED;
		}
		if (!end) {
			Print(L"Section %d has zero size\n", i);
			BS->FreePages(*alloc_address, *alloc_pages);
			return EFI_UNSUPPORTED;
		}

			if (Section->PointerToRawData < context.SizeOfHeaders) {
				Print(L"Section %d is inside image headers\n", i);
				BS->FreePages(*alloc_address, *alloc_pages);
				return EFI_UNSUPPORTED;
			}

			size = Section->Misc.VirtualSize;
			if (size > Section->SizeOfRawData)
				size = Section->SizeOfRawData;

			if (size > 0)
				CopyMem(base, data + Section->PointerToRawData, size);

			if (size < Section->Misc.VirtualSize)
				ZeroMem(base + size, Section->Misc.VirtualSize - size);
	}
	return 0;


}

// vim:fenc=utf-8:tw=75:noet
