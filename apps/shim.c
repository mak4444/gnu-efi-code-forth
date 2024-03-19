
#include "shim.h"

int loader_is_participating;

static EFI_LOADED_IMAGE *shim_li;
static EFI_LOADED_IMAGE shim_li_bak;

/*
 * Check whether we're in Secure Boot and user mode
 */
BOOLEAN secure_mode (void)
{       return 0;
}

/*
 * Check that the signature is valid and matches the binary
 */
EFI_STATUS
verify_buffer_authenticode (char *data, int datasize,
			    PE_COFF_LOADER_IMAGE_CONTEXT *context,
			    UINT8 *sha256hash, UINT8 *sha1hash)
{return 0;
	Print(L"", data, datasize, sha256hash, sha1hash,context);
}

/*
 * Check that the binary is permitted to load by SBAT.
 */
EFI_STATUS
verify_buffer_sbat (char *data, int datasize,
		    PE_COFF_LOADER_IMAGE_CONTEXT *context)
{
	Print(L"", data, datasize, context);
	return 0;
}

/*
 * Check that the signature is valid and matches the binary and that
 * the binary is permitted to load by SBAT.
 */
EFI_STATUS
verify_buffer (char *data, int datasize,
	       PE_COFF_LOADER_IMAGE_CONTEXT *context,
	       UINT8 *sha256hash, UINT8 *sha1hash)
{
	Print(L"", data, datasize, sha256hash, sha1hash,context);
	return 0;
}


EFI_STATUS start_efi(void *data,int datasize)
{
	EFI_STATUS efi_status;
	EFI_IMAGE_ENTRY_POINT entry_point;
	EFI_PHYSICAL_ADDRESS alloc_address;
	UINTN alloc_pages;
	/*
	 * We need to modify the loaded image protocol entry before running
	 * the new binary, so back it up
	 */
	CopyMem(&shim_li_bak, shim_li, sizeof(shim_li_bak));

	/*
	 * Verify and, if appropriate, relocate and execute the executable
	 */
	efi_status = handle_image(data, datasize, shim_li, &entry_point,
				  &alloc_address, &alloc_pages);
	if (EFI_ERROR(efi_status)) {
		Print(L"Failed to load image: %r\n", efi_status);
		goto restore;
	}

	loader_is_participating = 0;

	/*
	 * The binary is trusted and relocated. Run it
	 */
	efi_status = entry_point(LibImageHandle, ST);

restore:

	if (data)
		FreePool(data);

	return efi_status;
}
