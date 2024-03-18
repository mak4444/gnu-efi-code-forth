// SPDX-License-Identifier: BSD-2-Clause-Patent
/*
 * load-options.c - all the stuff we need to parse the load options
 */

#include "shim.h"

CHAR16 *second_stage;
void *load_options;
UINT32 load_options_size;

/*
 * Generate the path of an executable given shim's path and the name
 * of the executable
 */
EFI_STATUS
generate_path_from_image_path(EFI_LOADED_IMAGE *li,
			      CHAR16 *ImagePath,
			      CHAR16 **PathName)
{
	EFI_DEVICE_PATH *devpath;
	unsigned int i;
	int j, last = -1;
	unsigned int pathlen = 0;
	EFI_STATUS efi_status = EFI_SUCCESS;
	CHAR16 *bootpath;

	/*
	 * Suuuuper lazy technique here, but check and see if this is a full
	 * path to something on the ESP.  Backwards compatibility demands
	 * that we don't just use \\, because we (not particularly brightly)
	 * used to require that the relative file path started with that.
	 *
	 * If it is a full path, don't try to merge it with the directory
	 * from our Loaded Image handle.
	 */
	if (StrSize(ImagePath) > 5 && StrnCmp(ImagePath, L"\\EFI\\", 5) == 0) {
		*PathName = StrDuplicate(ImagePath);
		if (!*PathName) {
			Print(L"Failed to allocate path buffer\n");
			return EFI_OUT_OF_RESOURCES;
		}
		return EFI_SUCCESS;
	}

	devpath = li->FilePath;

	bootpath = DevicePathToStr(devpath);

	pathlen = StrLen(bootpath);

	/*
	 * DevicePathToStr() concatenates two nodes with '/'.
	 * Convert '/' to '\\'.
	 */
	for (i = 0; i < pathlen; i++) {
		if (bootpath[i] == '/')
			bootpath[i] = '\\';
	}

	for (i=pathlen; i>0; i--) {
		if (bootpath[i] == '\\' && bootpath[i-1] == '\\')
			bootpath[i] = '/';
		else if (last == -1 && bootpath[i] == '\\')
			last = i;
	}

	if (last == -1 && bootpath[0] == '\\')
		last = 0;
	bootpath[last+1] = '\0';

	if (last > 0) {
		for (i = 0, j = 0; bootpath[i] != '\0'; i++) {
			if (bootpath[i] != '/') {
				bootpath[j] = bootpath[i];
				j++;
			}
		}
		bootpath[j] = '\0';
	}

	for (i = 0, last = 0; i < StrLen(ImagePath); i++)
		if (ImagePath[i] == '\\')
			last = i + 1;

	ImagePath = ImagePath + last;
	*PathName = AllocatePool(StrSize(bootpath) + StrSize(ImagePath));

	if (!*PathName) {
		Print(L"Failed to allocate path buffer\n");
		efi_status = EFI_OUT_OF_RESOURCES;
		goto error;
	}

	*PathName[0] = '\0';
	if (StrnCaseCmp(bootpath, ImagePath, StrLen(bootpath)))
		StrCat(*PathName, bootpath);
	StrCat(*PathName, ImagePath);

error:
	FreePool(bootpath);

	return efi_status;
}

