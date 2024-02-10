STRUCTURES{
0
    2 FIELD     Year;       \ 1998 - 20XX
    1 FIELD     Month;      \ 1 - 12
    1 FIELD     Day;        \ 1 - 31
    1 FIELD     Hour;       \ 0 - 23
    1 FIELD     Minute;     \ 0 - 59
    1 FIELD     Second;     \ 0 - 59
    1 FIELD     Pad1;
    4 FIELD     Nanosecond; \ 0 - 999,999,999
    2 FIELD     TimeZone;   \ -1440 to 1440 or 2047
    1 FIELD     Daylight;
    1 FIELD     Pad2;
 CONSTANT /EFI_TIME

}STRUCTURES
