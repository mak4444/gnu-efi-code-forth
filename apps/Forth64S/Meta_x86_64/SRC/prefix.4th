REQUIRE GASM64_MOD ~mak\ARM7\SRC\gasm64.4th

MODULE: GASM64_MOD

[IFNDEF] M! : M! ! ; [THEN]

ALSO FORTH

' NOOP VALUE INCT_V

: A; 
 INCT_V
 ['] NOOP TO INCT_V
 EXECUTE
 ;

[UNDEFINED] ASM_INTERPRET
[IF]  : ASM_INTERPRET INTERPRET ;
[THEN]

: P:_INTERPRET ( cfa -- ... )
    >R  A;  R> TO INCT_V
       >IN M@ >R 
\ + TRACE  ." >ASM_INT"
\ + TRACE TRGO TRACE
    ASM_INTERPRET
               R> >IN M@ >R >IN M!  \ 
       A;      R> >IN M!           \ 
;

: P: >IN M@ '
  SWAP  >IN M! PARSE-NAME 1- CREATED , POSTPONE \
 DOES>
\ + TRACE  TRACE
 M@
\ + TRACE ." P:=" DUP H.
 P:_INTERPRET ;

PREVIOUS

P: POP,
P: PUSH,
P: add,
P: or,
P: adc,
P: sbb,
P: AND,
P: sub,
P: xor,
P: cmp,
P: test,
P: xchg,
P: lea,
P: mov,
P: movb,
P: movl,
P: movq,
P: movabs,
P: movzb,
P: movzbl,
P: movzbq,
P: movzw,
P: movzwl,
P: movzwq,

P: movsb,
P: movsbl,
P: movsbq,
P: movsw,
P: movswl,
P: movswq,
P: movslq,

P: cmovo,
P: cmovno,
P: cmovb,
P: cmovae,
P: cmove, 
P: cmovne,
P: cmovbe,
P: cmova, 
P: cmovs, 
P: cmovns,
P: cmovp, 
P: cmovnp,
P: cmovl, 
P: cmovge,
P: cmovle,
P: cmovg, 
: cmovc	cmovb ;

P: btc,

P: not,
P: neg,
P: mul,
P: imul,
P: div,
P: idiv,

P: notb,
P: negb,
P: mulb,
P: imulb,
P: divb,
P: idivb,

P: notw,
P: negw,
P: mulw,
P: imulw,
P: divw,
P: idivw,

P: notl,
P: negl,
P: mull,
P: imull,
P: divl,
P: idivl,
  
P: notq,
P: negq,
P: mulq,
P: imulq,
P: divq,
P: idivq,

P: sldt,
P: sgdt,
P: prefetch,
P: seto, 
P: setno,
P: setb, 
P: setae,
P: sete, 
P: setne,
P: setbe,
P: seta,
P: sets, 
P: setns,
P: setp, 
P: setnp,
P: setl, 
P: setge,
P: setle,
P: setg, 
P: fxsave,

P: inc,
P: dec,
P: bswap,

P: INCL,
P: DECL,
P: INCQ,
P: DECQ,

P: lss,
P: lfs,
P: lgs,

\ P: btr,


P: rol,
P: ror,
P: rcl,
P: rcr,
P: shl,
P: shr,
P: shl,
P: sar,

P: rolb,
P: rorb,
P: rclb,
P: rcrb,
P: shlb,
P: shrb,
P: salb,
P: sarb,

P: rolw,
P: rorw,
P: rclw,
P: rcrw,
P: shlw,
P: shrw,
P: salw,
P: sarw,

P: roll,
P: rorl,
P: rcll,
P: rcrl,
P: shll,
P: shrl,
P: sall,
P: sarl,

P: rolq,
P: rorq,
P: rclq,
P: rcrq,
P: shlq,
P: shrq,
P: salq,
P: sarq,


P: CALL,
P: JMP,
P: jo,
P: jno,
P: jb,
P: jae,
P: je,
P: jz,
P: jne,
P: jbe,
P: ja,
P: js,
P: jns,
P: jp,
P: jnp,
P: jl,
P: jge,
P: jle,
P: jg,
P: loopne,
P: loope,
P: loop,
P: jrcxz,

;MODULE
