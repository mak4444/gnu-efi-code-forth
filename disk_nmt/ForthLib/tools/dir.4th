
  /EFI_FILE_INFO 512 2* + CONSTANT FILEINFO_SIZE
CREATE FILEINFO  FILEINFO_SIZE ALLOT
: $DIR ( addr len -- )
  R/O  OPEN-FILE THROW >R
  BEGIN FILEINFO FILEINFO_SIZE R@ READ-FILE THROW 
  WHILE FILEINFO FI.FileName UZTYPE 9 EMIT
	FILEINFO FI.Attribute @ EFI_FILE_DIRECTORY  AND IF ." <dir>" THEN  CR
  REPEAT  
  R>  CLOSE-FILE DROP
;

: DIR PARSE-NAME $DIR ;
