
CREATE RUTAB

  0 C,  1 C,  2 C,  3 C,  4 C,  5 C,  6 C,  7 C,
  8 C,  9 C, $A C, $B C, $C C, $D C, $E C, $F C,

  $10 C, $11 C, $12 C, $13 C, $14 C, $15 C, $16 C, $17 C,
  $18 C, $19 C, $1A C, $1B C, $1C C, $1D C, $1E C, $1F C,
                             
  $20 C, $21 C, 'ü' C, $23 C, $24 C, $25 C, $26 C, 'Ü' C,
  $28 C, $29 C, $2A C, $2B C, 'Â' C, $2D C, 'À' C, $2F C,
                             
  $30 C, $31 C, $32 C, $33 C, $34 C, $35 C, $36 C, $37 C,
  $38 C, $39 C, 'ö' C, 'Ö' C, 'â' C, $3D C, 'à' C, $3F C,

  $40 C, 'æ' C, 'é' C, 'ó' C, '÷' C, 'õ' C, 'á' C, 'ð' C,
  'ò' C, 'û' C, 'ï' C, 'ì' C, 'ä' C, 'ø' C, 'ô' C, 'ý' C,
                             
  'ú' C, 'ê' C, 'ë' C, 'ù' C, 'å' C, 'ç' C, 'í' C, 'ã' C,
  'þ' C, 'î' C, 'ñ' C, 'È' C, $5C C, 'ß' C, $5E C, $5F C,
                             
  $60 C, 'Æ' C, 'É' C, 'Ó' C, '×' C, 'Õ' C, 'Á' C, 'Ð' C,
  'Ò' C, 'Û' C, 'Ï' C, 'Ì' C, 'Ä' C, 'Ø' C, 'Ô' C, 'Ý' C,

  'Ú' C, 'Ê' C, 'Ë' C, 'Ù' C, 'Å' C, 'Ç' C, 'Í' C, 'Ã' C,
  'Þ' C, 'Î' C, 'Ñ' C, 'è' C, $7C C, 'ÿ' C, $7E C, $7F C,

  $80 C, $81 C, $82 C, $83 C, $84 C, $85 C, $86 C, $87 C,
  $88 C, $89 C, $8A C, $8B C, $8C C, $8D C, $8E C, $8F C,

  $90 C, $91 C, $92 C, $93 C, $94 C, $95 C, $96 C, $97 C,
  $98 C, $99 C, $9A C, $9B C, $9C C, $9D C, $9E C, $9F C,

  $A0 C, $A1 C, $A2 C, $A3 C, $A4 C, $A5 C, $A6 C, $A7 C,
  $A8 C, $A9 C, $AA C, $AB C, $AC C, $AD C, $AE C, $AF C,

  $B0 C, $B1 C, $B2 C, $B3 C, $B4 C, $B5 C, $B6 C, $B7 C,
  $B8 C, $B9 C, $BA C, $BB C, $BC C, $BD C, $BE C, $BF C,

  $C0 C, $C1 C, $C2 C, $C3 C, $C4 C, $C5 C, $C6 C, $C7 C,
  $C8 C, $C9 C, $CA C, $CB C, $CC C, $CD C, $CE C, $CF C,

  $D0 C, $D1 C, $D2 C, $D3 C, $D4 C, $D5 C, $D6 C, $D7 C,
  $D8 C, $D9 C, $DA C, $DB C, $DC C, $DD C, $DE C, $DF C,

  $E0 C, $E1 C, $E2 C, $E3 C, $E4 C, $E5 C, $E6 C, $E7 C,
  $E8 C, $E9 C, $EA C, $EB C, $EC C, $ED C, $EE C, $EF C,
                             
  $F0 C, $F1 C, $F2 C, $F3 C, $F4 C, $F5 C, $F6 C, $F7 C,
  $F8 C, $F9 C, $FA C, $FB C, $FC C, $FD C, $FE C, $FF C,
                             
0 VALUE RUTAB?
: RUKEY ( -- c )
  BEGIN [ ' KEY DEFER@ COMPILE, ] DUP KEY2_F10 = \
  WHILE DROP  RUTAB? 0= TO RUTAB?
  REPEAT
  DUP $FF U> IF BREAK
 RUTAB? IF RUTAB + C@ THEN
 ; 

' RUKEY TO KEY

 