REQUIRE ASM64_MOD ~mak/gasm64/asm64.f 

' NOOP VALUE INCT_V

: A; 
 INCT_V
 ['] NOOP TO INCT_V
 EXECUTE
 ;

MODULE: ASM64_MOD

[IFNDEF] M! : M! ! ; [THEN]

ALSO FORTH


[UNDEFINED] ASM_INTERPRET
[IF]  : ASM_INTERPRET INTERPRET ;
[THEN]

: P:_INTERPRET ( cfa -- ... )
    >R  A;  R> TO INCT_V
       >IN M@ >R  ASM_INTERPRET
		R> >IN M@ >R >IN M!
	A;	R> >IN M!
;

: P: >IN M@ '
  SWAP  >IN M! PARSE-NAME 1- CREATED , POSTPONE \
 DOES> M@ P:_INTERPRET ;

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
P: mov,
P: lea,
P: movzb,

P: not,
P: neg,
P: mul,
P: imul,
P: div,
P: idiv,

P: inc,
P: dec,

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

P: jo,
P: jno,
P: jb,
P: jae,
P: je,
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
P: ret,
P: retq,
P: cwtl,
P: cltd,
P: fwait,
P: pushfq,
P: popfq, 
P: sahf,  
P: lahf,
P: lret,
P: int3,
P: icebp,
P: hlt,
P: cmc,
P: clc,
P: stc,
P: cli,
P: sti,
P: cld,
P: std,

;MODULE

