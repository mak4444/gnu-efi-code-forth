REQUIRE >DPF ~mak\LIB\THERE\pfincl.f 

: PF_CLOSE
	#CLOSE OUTPUT
	>DPF DPF> ;

: PF_OPEN  (  c-addr u fam -- fileid ior )
	#OPEN OUTPUT
	>DPF >$PF
	DPF>  DPF>
\ CR ." OPEN="  2DUP ._ ._
 ;


: PF_R-LINE ( c-addr u1 fileid -- u2 flag ior )
	#R-LINE OUTPUT
	>DPF >PF
	SPF> NIP DPF> DPF> ;

: PF_F-POS  ( fileid -- ud ior )
	#F-POS OUTPUT
	>DPF DPF> DPF> DPF> ;

: PF_F-RPOS ( ud fileid -- ior )
	#F-RPOS OUTPUT
	>DPF >DPF >DPF DPF> ;
