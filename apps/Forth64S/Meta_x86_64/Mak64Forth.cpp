#include <iostream>
#include <iomanip>

#include <climits>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <fstream>
#include <linux/types.h>
#include <vector>
#include <curses.h>

#include <termios.h>

using namespace std;

#define LOGD printf("\n"); printf

#define STACK_SIZE 1000 /* cells reserved for the stack */
#define RSTACK_SIZE 1000 /* cells reserved for the return stack */
#define HERE_SIZE1 100000

typedef long unsigned Cell;
typedef long sCell;

#ifndef __u128
typedef __int128 unsigned __u128;
#endif

#define pp(cName) const sCell p##cName = ~(sCell)cName;
#define PP(cName) const sCell p##cName = (sCell)cName;

sCell StackArea[STACK_SIZE] ;
sCell RStackArea[RSTACK_SIZE] ;
sCell *Stack = &StackArea[STACK_SIZE-8] ;
sCell *rStack = &RStackArea[RSTACK_SIZE-8] ;
sCell HereArea[HERE_SIZE1];
typedef  void (*proc)(void);

sCell * here = HereArea ;

static string CmdString;
static string ReturnString;

void  MakeImag(void);
Cell numericBase = 10;

sCell ireg = ~(sCell)MakeImag;
sCell * ip ;

vector <Cell> InVector;
vector <string> TibVector;
streambuf * coutbuf; 

void  Noop(void) {}  pp(Noop)

sCell Tos;
sCell * Handler = NULL;
const char * ARGV1;

void Co( sCell cod ){ *here++ =  cod ;}
void Co( sCell cod1,sCell cod2 ){    Co(cod1); Co(cod2); }
void Co( sCell cod1,sCell cod2,sCell cod3 ){    Co(cod1,cod2); ; Co(cod3); }
void Co( sCell cod1,sCell cod2,sCell cod3,sCell cod4 ){  Co(cod1,cod2,cod3); ; Co(cod4); }
void Co( sCell cod1,sCell cod2,sCell cod3,sCell cod4,sCell cod5 )
{ Co(cod1,cod2,cod3,cod4); ; Co(cod5); }
void Co( sCell cod1,sCell cod2,sCell cod3,sCell cod4,sCell cod5,sCell cod6 )
{ Co(cod1,cod2,cod3,cod4,cod5); Co(cod6); }
void Co( sCell cod1,sCell cod2,sCell cod3,sCell cod4,sCell cod5,sCell cod6,sCell cod7 )
{ Co(cod1,cod2,cod3,cod4,cod5,cod6);  Co(cod7);}


void  DoVar(void){   *--Stack= Tos; Tos =(sCell)ip; ip = (sCell*)*rStack++; } pp(DoVar)
void  DoConst(void){ *--Stack= Tos; Tos = *ip; ip = (sCell*)*rStack++; } pp(DoConst)
void Execute()
    {   ireg =Tos; Tos=*Stack++;
	if ( (ireg<0) ^ (pNoop>0) ) {
	((proc) (~ireg))();
	return;
    	}
    *--rStack = (sCell) ip;
        ip = (sCell *) ireg;
    } pp(Execute)

void DoDefer(){
    ireg = *ip;
    if ( (ireg<0) ^ (pNoop>0) ) {
        ip = (sCell*)*rStack++; // exit
        ((proc)(~ireg))();
            return;
        }
         ip =  (sCell *) ireg;

} pp(DoDefer)

void Lit(){  *--Stack = Tos; Tos = *ip++; } pp(Lit)
void Lit( sCell val) {Co(pLit);  *here++ = val;  }
void Compile(){  *here++ = Tos; Tos = *Stack++;  }  pp(Compile)
void LitCo(){  Co(pLit); Compile();}  pp(LitCo)
void Allot(){ *(sCell*)&here += Tos; Tos = *Stack++;  }  pp(Allot)

void  Exit() { ip = (sCell*)*rStack++; } pp(Exit)
void  Here(void){  *--Stack = Tos; Tos = (sCell)here;  } pp(Here)

void Branch(){ ip = *(sCell**)ip; } pp(Branch)

void QBranch(){
    if(Tos) ip++;
    else    ip = *(sCell**)ip;
    Tos =   *Stack++;
} pp(QBranch)

void Str() {
    *--Stack = Tos;
    *--Stack = (Cell)ip+1;
        Tos = *(__u8 *)ip;
    ip = (sCell*) ((Cell)ip + Tos + 1);
    ip = (sCell*) ( ( (Cell)ip + sizeof(Cell) - 1 ) & (-sizeof(Cell) )  );

}  pp(Str)

void Dup(){   *--Stack= Tos;  } pp(Dup)
void Drop(){  Tos = *Stack++;  } pp(Drop)
void Nip(){   Stack++;   } pp(Nip)
void QDup(){   if(Tos) *--Stack= Tos;   } pp(QDup)
void Over(){   *--Stack= Tos; Tos = Stack[1];    } pp(Over)
void Tuck(){    sCell tt=*Stack; *Stack=Tos; *--Stack=tt;  }  pp(Tuck)
void Pick(){    Tos = Stack[Tos];  }  pp(Pick)
void i2dup(){   *--Stack= Tos; *--Stack= Stack[1];  } pp(i2dup)
void i2over(){   *--Stack= Tos;  *--Stack= Stack[3]; Tos= Stack[3];  } pp(i2over)
void i2drop(){  Stack++; Tos = *Stack++; } pp(i2drop)
void Swap(){  sCell tt=Tos; Tos=Stack[0]; Stack[0]=tt;  }  pp(Swap)
void i2Swap(){
    sCell   tt=Tos; Tos=Stack[1]; Stack[1]=tt;
            tt=Stack[0]; Stack[0]=Stack[2]; Stack[2]=tt;  }  pp(i2Swap)
void Rot(){ Cell tt=Stack[1]; Stack[1]=Stack[0]; Stack[0]=Tos; Tos=tt; } pp(Rot)
void Add(){ Tos += *Stack++;  } pp(Add)
void Sub(){ Tos = -Tos;  Tos += *Stack++; } pp(Sub)
void Negate(){ Tos = -Tos; } pp(Negate)
void Invert(){ Tos = ~Tos; } pp(Invert)
void i1Add(){ Tos++; } pp(i1Add)
void i1Sub(){ Tos--; } pp(i1Sub)
void i2Add(){ Tos +=2; } pp(i2Add)
void i2Sub(){ Tos -=2; } pp(i2Sub)
void Mul(){ Tos *= *Stack++; } pp(Mul)
void Div(){ sCell tt=*Stack++; Tos = tt/Tos; } pp(Div)
void i2Mul(){ Tos *= 2; } pp(i2Mul)
void i2Div(){ Tos /= 2; } pp(i2Div)
void Mod(){ sCell tt=*Stack++; Tos = tt%Tos; } pp(Mod)
void UMul(){ Tos = (Cell) Tos * (Cell) *Stack++; } pp(UMul)
void UDiv(){ Cell tt=*Stack++; Tos = tt/(Cell)Tos; } pp(UDiv)
void And(){ Tos &= *Stack++; } pp(And)
void Or(){  Tos |= *Stack++; } pp(Or)
void Xor(){ Tos ^= *Stack++; } pp(Xor)
void ARshift(){  Tos = *Stack++ >> Tos ; } pp(ARshift)
void Rshift(){  Tos = *(Cell*)Stack++ >> Tos ; } pp(Rshift)
void Lshift(){  Tos = *Stack++ << Tos ; } pp(Lshift)

void HDot(){
    cout << std::setbase(static_cast<Cell>(16)) << static_cast<sCell>(Tos) << " "  ;
    Tos = *Stack++;
}   pp(HDot)

void UDot(){
    cout << std::setbase(static_cast<Cell>(numericBase)) << static_cast<Cell>(Tos) << " "  ;
    Tos = *Stack++;
}   pp(UDot)

void Dot(){
//    cout << std::setbase(static_cast<sCell>(numericBase)) << static_cast<sCell>(Tos) << " "  ;
//    Tos = *Stack++;
	if(Tos<0) { cout << '-' ; Tos=-Tos; }
	UDot();
}   pp(Dot)

void Load(){  Tos =  *(Cell*)Tos;  } pp(Load)
void Store(){ *(Cell*)Tos = *Stack++;  Tos = *Stack++;} pp(Store)
void CLoad(){ Tos =  (Cell)*(__u8*) Tos;  } pp(CLoad)
void CStore(){ *(__u8*)Tos = (__u8)*Stack++; Tos = *Stack++;  } pp(CStore)
void CStoreA(){ *(__u8*)Tos = (__u8)*Stack++; } pp(CStoreA)
void WLoad(){ Tos =  (Cell)*(__u16*) Tos;  } pp(WLoad)
void WStore(){ *(__u16*)Tos = (__u16)*Stack++; Tos = *Stack++;  } pp(WStore)
void LLoad(){ Tos =  (Cell)*(__u32*) Tos;  } pp(LLoad)
void LStore(){ *(__u32*)Tos = (__u32)*Stack++; Tos = *Stack++;  } pp(LStore)

void i2Store(){
 __u128 val = ((__u128)(Cell)Stack[1]<<64) + (__u128)(Cell)Stack[0];
   *(__u128*)Tos = val;
   Stack += 2 ;  Tos = *Stack++;} pp(i2Store)
void i2Load(){  __u128 val = *(__u128*)Tos;	Tos= val;	*--Stack=val>>64;  } pp(i2Load)
void AddStore(){ *(Cell*)Tos += *Stack++;  Tos = *Stack++;} pp(AddStore)
void Count(){ *--Stack = Tos+1; Tos = (sCell) *(char *)Tos; } pp(Count)
void On(){  *(Cell*)Tos = -1; Tos = *Stack++; } pp(On)
void Off(){ *(Cell*)Tos = 0; Tos = *Stack++;  } pp(Off)
void Incr(){  *(Cell*)Tos += 1; Tos = *Stack++; } pp(Incr)
void ZEqual(){ Tos = -(Tos==0); } pp(ZEqual)
void ZNEqual(){ Tos = -(Tos!=0); } pp(ZNEqual)
void DZEqual(){  Tos = -( (Tos | *Stack++) == 0); } pp(DZEqual)
void ZLess(){ Tos = -(Tos<0); } pp(ZLess)
void Equal(){  Tos = -(*Stack++==Tos); } pp(Equal)
void NEqual(){  Tos = -(*Stack++!=Tos); } pp(NEqual)
void Less(){   Tos = -(*Stack++<Tos);  } pp(Less)
void Great(){  Tos = -(*Stack++>Tos);  } pp(Great)
void ULess(){  Tos = -((Cell)*Stack++ < (Cell)Tos); } pp(ULess)
void UGreat(){ Tos = -((Cell)*Stack++ > (Cell)Tos); } pp(UGreat)

void Max(){ sCell tt = *Stack++; if(tt>Tos) Tos=tt; } pp(Max)
void Min(){ sCell tt = *Stack++; if(tt<Tos) Tos=tt; } pp(Min)
void i0Max(){  if(Tos<0) Tos=0; } pp(i0Max)

void ToR(){   *--rStack = Tos; Tos = *Stack++; }	pp(ToR)
void RLoad(){ *--Stack = Tos; Tos = *rStack; }		pp(RLoad)
void FromR(){ *--Stack = Tos; Tos = *rStack++; }	pp(FromR)
void i2ToR(){  *--rStack = *Stack++; *--rStack = Tos ; Tos = *Stack++; } pp(i2ToR)
void i2RLoad(){ *--Stack = Tos; Tos = *rStack; *--Stack = rStack[1];	  } pp(i2RLoad)
void i2FromR(){ *--Stack = Tos; Tos = *rStack++; *--Stack = *rStack++;	  } pp(i2FromR)
void RDrop(){ *rStack++; }    pp(RDrop)
void RPGet(){ *--Stack = Tos; Tos = (Cell) rStack; } pp(RPGet)
void SPGet(){ *--Stack = Tos; Tos = (Cell) Stack ; } pp(SPGet)
void RPSet(){   rStack = (sCell*)Tos; Tos = *Stack++; } pp(RPSet)
void SPSet(){    Stack = (sCell*)(Tos+8); Tos = Stack[-1]; } pp(SPSet)


void Emit() { // cout << (char)Tos;
    cout.put(static_cast<char>(Tos));

 Tos = *Stack++; } pp(Emit)
void Space() { cout << " "; } pp(Space)
void Cr() { cout << endl; } pp(Cr)

void Type() {
    char* caddr = (char*) *Stack++;
    cout << string(caddr, Tos);
    Tos =  *Stack++;
} pp(Type)

void Ahead(){ Co( pBranch); *--Stack = Tos; Tos = (sCell)here; Co(0);} pp(Ahead)
void If(){ Co( pQBranch); *--Stack = Tos; Tos= (sCell)here; Co(0);} pp(If)
void Then(){  *(sCell**)Tos++ = here; Tos = *Stack++; } pp(Then)
void Else(){  Ahead();    Swap(); Then(); } pp(Else)
void Begin(){ *--Stack = Tos; Tos =  (sCell)here; } pp(Begin)
void Until(){ Co( pQBranch);   *here++ = (sCell)Tos; Tos = *Stack++; } pp(Until)
void Again(){ Co( pBranch);  *here++ = (sCell)Tos; Tos = *Stack++; } pp(Again)
void While(){ If(); Swap(); } pp(While)
void Repeat(){ Again(); Then(); }   pp(Repeat)

void DNegate(){ __int128 val =
 -(__int128)( ((__u128)(Cell)Tos<<64) + (__u128)(Cell)Stack[0] ) ;
	Tos= val>>64;
	Stack[0]=val;
  } pp(DNegate)

void DAbs(){   if(Tos<0) DNegate();  } pp(DAbs)

void DAdd()
{ __u128 sum= ((__u128)(Cell)Tos<<64) + (__u128)(Cell)Stack[0] +
	 ((__u128)(Cell)Stack[1]<<64) + (__u128)(Cell)Stack[2];
	Stack += 2 ;
	Tos= sum>>64;
	Stack[0]=sum;
} pp(DAdd)

void UMMul()
{ __u128 mul= (__u128)(Cell)Tos * (__u128)(Cell)Stack[0] ;
	Tos= mul>>64;
	Stack[0]=mul;
} pp(UMMul)

void UMMOD()
{ __u128 div= ((__u128)(Cell)Stack[0]<<64) + (__u128)(Cell)Stack[1];
	*++Stack = (sCell) ( div % (Cell)Tos ) ;
	Tos = (sCell) ( div / (Cell)Tos ) ;
} pp(UMMOD)

// hex 77  000000  88000000 000000  88000000 d+ h. h. h.
// hex 77 -1  0 -1 0  d+ h. h. h.

void Align()
{   Cell sz = ( sizeof (Cell) - 1 ) ;
    char * chere = (char *)here;
    while( (Cell) chere & sz ) *chere++ = 0 ;
    here = (sCell *)chere;
}
// CODE FILL ( c-addr u char -- ) \ 94
void Fill()
{    Cell len =  *Stack++;
    __u8 *adr = (__u8 *) *Stack++;
  while (len-- > 0)  *adr++ = (__u8)Tos;
  Tos =  *Stack++;
}  pp(Fill)

void Cmove()
{
  __u8 *c_to = (__u8 *) *Stack++;
  __u8 *c_from =(__u8 *) *Stack++;
  while (Tos-- > 0)
    *c_to++ = *c_from++;
  Tos =  *Stack++;
}  pp(Cmove)

void Cmove_up()
{
  __u8 *c_to = (__u8 *) *Stack++;
  __u8 *c_from =(__u8 *) *Stack++;
  while (Tos-- > 0)
    c_to[Tos] = c_from[Tos];
  Tos =  *Stack++;
}  pp(Cmove_up)


void StrComp(const char * s, sCell len)
{   char * chere = (char *)here;
    len &= (1 << CHAR_BIT) - 1 ;
    *chere++ = (char)len;                /* store count byte */
    while (--len >= 0)          /* store string */
        *chere++ = *s++;

    here = (sCell *)chere;
    Align();
}

void StrCmp(){  StrComp((char *) *Stack++, Tos); Tos = *Stack++; } pp(StrCmp)

void Tp(string s) {
    Co( pStr);
    StrComp(s.data(), s.length());
    Co( pType);;
}

void SpSet(){    Stack = (sCell*)*Stack; } pp(SpSet)

sCell  ForthWordlist[] = {0,0,0};

const Cell ContextSize = 10;
sCell * Context[ContextSize] = {ForthWordlist};
sCell * Current[] = {ForthWordlist};

sCell * Last;
sCell * LastCFA;


void  WordBuild (const char * name, sCell cfa )
{
    LastCFA=here;
    Co(cfa);
    Co(0); // flg
    Co(** (sCell **) Current);
    Last=here;
    StrComp(name, strlen(name));
}

void Smudge(){ **(sCell***) Current=Last; } pp(Smudge)

void Immediate(){ Last[-2] |= 1; } pp(Immediate)

void FthItem (string  name, sCell cfa ){
    WordBuild (name.data(), cfa );
    Smudge();
}

sCell Header(string name) {
    FthItem (name,0);
    *(sCell **)LastCFA = here;
    return  *(sCell *)LastCFA;
}

sCell Variable (string name ) {
    FthItem(name,0);
    *(sCell **) LastCFA = here;
    *here++ =  pDoVar;
    *here++ = 0;
    return  *(sCell *)LastCFA;
}

sCell Variable (string name, sCell val ) {
    FthItem(name,0);
    *(sCell **) LastCFA = here;
    *here++ =  pDoVar;
    *here++ = val;
    return  *(sCell *)LastCFA;
}

sCell Constant (string name, sCell val ) {
    FthItem(name,0);
    *(sCell **) LastCFA = here;
    *here++ = pDoConst;
    *here++ = val;
    return  *(sCell *)LastCFA;
}

void Source(){
 *--Stack = Tos;
  __u8 * tib = (__u8 *)CmdString.data();
   *--Stack = (sCell) tib;
  Tos = CmdString.length();
  if(Tos>0) if( tib[Tos-1] < ' ' ) Tos--;
  } pp(Source)

sCell i2in[] = { pDoVar , 0  }; PP(i2in)
sCell *v2in = (sCell *) &i2in[1];

sCell SourceId[] = { pDoConst, 0 }; PP(SourceId)

void AskE()
{   std::getline(std::cin, CmdString);
     *v2in=0;
      *--Stack = Tos; Tos = -1;
} pp(AskE)

void FileRefill()  // ( -- flg )
{   auto f = (std::fstream*) SourceId[1];
    *v2in=0;
     *--Stack = Tos; Tos = getline( *f, CmdString ) ? -1 : 0 ;
} pp(FileRefill)

void ParseName() {
    Cell addr,Waddr,Eaddr;
    Cell tib = (Cell) CmdString.data();
    Cell ntib = CmdString.length()-1;
    addr= *v2in + tib ;
    Eaddr= tib  + ntib;

    *--Stack = Tos;
    while (  addr<Eaddr ) { if( *(__u8*)addr > ' ') break;
        addr++; }
    *--Stack=Waddr=addr;
    *v2in = addr - tib;
    while ( addr<=Eaddr ) { (*v2in)++; if( *(__u8*)addr <= ' ') break;
     addr++; }
    Tos=addr-Waddr;
} pp(ParseName)

void Parse() {
    Cell addr,Waddr,Eaddr;
    Cell tib = (Cell) CmdString.data();
    Cell ntib = CmdString.length()-1;
	if(((__u8*)tib)[ntib] == '\r' ) ntib--;
    addr= *v2in + tib ;
    Eaddr= tib  + ntib;

    char cc = (char)Tos;
    *--Stack=Waddr=addr;
    while ( addr<=Eaddr ) {  (*v2in)++;  if(*(__u8*)addr == cc ) break;
        addr++;}
    Tos=addr-Waddr;
} pp(Parse)

#ifndef memcasecmp
Cell memcasecmp (const void *vs1, const void *vs2, Cell n)
{
    unsigned int i;
    unsigned char const *s1 = (unsigned char const *) vs1;
    unsigned char const *s2 = (unsigned char const *) vs2;
    for (i = 0; i < n; i++)
    {
        unsigned char u1 = *s1++;
        unsigned char u2 = *s2++;
        if (toupper (u1) != toupper (u2))
            return toupper (u1) - toupper (u2);
    }
    return 0;
}
#endif

Cell CCompare( void * caddr1  ,  Cell len1 ,  void * caddr2  ,  Cell len2) {
    if (len1 < len2) return -1;
    if (len1 > len2) return  1;

//    auto cmpResult = std::memcmp(caddr1, caddr2, len1);
    auto cmpResult = memcasecmp(caddr1, caddr2, len1);

    if (cmpResult < 0) return -1;
    if (cmpResult > 0) return  1;
    return   0;
}

void Compare(){ 
	char * caddr1 = (char *) *Stack++;
	sCell  len1 =  *Stack++;
	char * caddr2 = (char *) *Stack++;

    if (len1 != Tos) {  Tos -= len1; return; }

    Tos = std::memcmp(caddr1, caddr2, Tos);  } pp(Compare)

void UCompare(){ 
	char * caddr1 = (char *) *Stack++;
	sCell  len1 =  *Stack++;
	char * caddr2 = (char *) *Stack++;

    if (len1 != Tos) {  Tos -= len1; return; }

    Tos = memcasecmp(caddr1, caddr2, Tos);  } pp(UCompare)

char *SEARCH(char **wid,  char * word , Cell len)
{ char * addr= (char *) *wid;
    for(;;)
    {   if(!addr) return NULL;
        char * caddr = addr ;
        if( !CCompare(word, len, caddr+1, *caddr ))
            return  addr;
        addr = ((char **)addr)[-1];
    }
}

void FromName(){  Tos=((sCell *)Tos)[-3]; } pp(FromName)

void SearchWordList() // ( c-addr u wid --- 0 | xt 1 xt -1 )
{
    char ** addr=  (char **) Tos;
    Cell  len=Stack[0];
    char * word= (char * ) Stack[1];

    if(!addr) { Stack+=2; Tos=0; return; }
    Cell * nfa= (Cell*) SEARCH(addr,word,len);
    if(!nfa) {
        Stack+=2; Tos=0;
        return;
    }
    Stack++;
    Stack[0]=nfa[-3];
    Tos = nfa[-2]&1 ? 1 : -1;

}  pp(SearchWordList)

void SFind()
{	sCell * voc=  (sCell *) Context;
    *--Stack = Tos;
    while( *voc )
    {	*--Stack = Stack[1];
        *--Stack = Stack[1]; Tos=*voc;
        SearchWordList();
        if(Tos)
        {   Stack[2]=Stack[0];  Stack+=2; // 2nip
            return;
        }   voc++;
    }

} pp(SFind)

Cell State;

void StateQ(){ *--Stack= Tos; Tos = State; } pp(StateQ)

void IMode(){ State = 0;}  pp(IMode)
void CMode(){ State = -1;}  pp(CMode)

sCell * YDP;
sCell * YDP0;

sCell YDPFL[] = { pDoConst, 0 }; PP(YDPFL)

void QYDpDp()
{
  if(YDPFL[1] == 0) return;
   sCell * tmp = YDP ;
    YDP = here ;
    here = tmp ;
}

void SBuild()
{    char * name = (char * ) *Stack++ ;
	QYDpDp();
    LastCFA=here;
    Co(0);
    Co(0); // flg
    Co(** (sCell  **) Current);
    Last=here;
    StrComp(name, Tos);
    Tos = *Stack++;
	QYDpDp();
    *(sCell **)LastCFA = here;
}

void Build()
{ //   *--Stack = Tos; Tos=(sCell)pNoop;
    ParseName();
    SBuild();
} pp(Build)

void SHeader()
{
	SBuild();
	Smudge();  
} pp(SHeader)

void SNunber0() // ( str len -- m flg )
{
    char* rez;
    char NumStr[44];
    auto len = Tos;
    auto caddr = (char*) Stack[0];
    NumStr[len]=0;
    while(len){ --len; NumStr[len] = caddr[len]; }
    Stack[0] = strtol( NumStr,  &rez, numericBase);
    Tos =  strlen(rez);
}  pp(SNunber0)

void Colon(){
  Build();
  CMode(); } pp( Colon)
void Semicolon(){ Co(pExit); Smudge(); IMode(); } pp(Semicolon)

void to_catch(){
    *--rStack = (sCell)Handler;
    *--rStack = (sCell)Stack;
    Handler = rStack;
    Execute();
} pp(to_catch)

void from_catch(){
    rStack++;
    Handler = (sCell*)*rStack++;
    *--Stack = Tos;  Tos = 0;
    ip = (sCell*)*rStack++; // exit
} pp(from_catch)

sCell Catch[] = { pto_catch, pfrom_catch }; PP(Catch)

void FThrowDo()
{   *--Stack = Tos;
    if (Handler == NULL); //  TODO("Handler=0")
    rStack =   Handler ;
    Stack = (sCell*)*rStack++;
    Handler = (sCell*)*rStack++;
    ip = (sCell * ) *rStack++;
}

void FThrow(){
    if (Tos == 0){  Tos = *Stack++; return;  }
    FThrowDo();
} pp(FThrow)

sCell Lastin =0;
sCell SaveErrQ = -1;
string ErrorCmd;
string ErrorWord;
string ErrorFile;
sCell ErrIn;

void SaveError()
{ if(SaveErrQ & Tos )
    {  auto fs = (std::fstream*) SourceId[1];
  SaveErrQ = 0;
       ErrIn = *v2in ;
//	if(SourceId[1]) ErrorFile<< *(std::fstream*) SourceId[1];
//	else ErrorFile = "";
	if(CmdString.length()<222) { ErrorCmd = CmdString; }
	else ErrorCmd = "";
	if(*v2in < Lastin) Lastin=0;
       ErrorWord = string( CmdString.data() + Lastin ,  *v2in - Lastin );
//    	cout << "\nSaveError=<" << fs << ErrorWord << ">\n";
    }

} pp(SaveError)

void PrintErr()
{  numericBase = 10;
   cerr << "\n" << ErrorCmd << "\n" << ErrorWord << "\n ERR=" <<
    std::setbase(static_cast<int>(10)) << static_cast<sCell>(Tos)  ;
     Tos = *Stack++;
     SaveErrQ=-1;
} pp(PrintErr)

// R/O ( -- fam )
void readOnly() { *--Stack = Tos; Tos = (sCell)(std::ios_base::in); }  pp(readOnly)

// R/W ( -- fam )
void readWrite() { *--Stack = Tos; Tos =(sCell)(std::ios_base::in|std::ios_base::out); } pp(readWrite)

// W/O ( -- fam )
void writeOnly() { *--Stack = Tos; Tos =(sCell)(std::ios_base::out); } pp(writeOnly)

// BIN ( fam1 -- fam2 )
void bin() { *--Stack = Tos; Tos = *Stack | (sCell)(std::ios_base::binary); } pp(bin)

// CREATE-FILE ( c-addr u fam -- fileid ior )
void createFile() {
    auto caddr = (char*)(Stack[1]);
    auto len = Stack[0];
    auto fam = static_cast<std::ios_base::openmode>(Tos);
    Tos = *Stack++;
    auto f = new std::fstream( string(caddr, len) , fam | std::ios_base::trunc);

    if (f->is_open()) {
        Stack[0] =(sCell)f;
        Tos = 0;
    }
    else {
        delete f;
        Stack[0] = 0;
        Tos = -1;
    }
} pp(createFile)

// OPEN-FILE ( c-addr u fam -- fileid ior )

void openFile() {
    auto caddr = (char*)(Stack[1]);
    auto len = Stack[0];
    auto fam = static_cast<std::ios_base::openmode>(Tos);
    Tos = *Stack++;

    auto f = new std::fstream( string(caddr, len) , fam );

    if (f->is_open()) {
        Stack[0] =(sCell)f;
        Tos = 0;
    }
    else {
        delete f;
        Stack[0] = 0;
        Tos = -1;
    }

} pp(openFile)

// CLOSE-FILE ( fileid -- ior )
void closeFile() {
    auto f = (std::fstream*)Tos;
    if (f == nullptr) { Tos = -1;; return; }
    f->close();
    delete f;
    Tos = 0;

} pp(closeFile)

// READ-FILE ( c-addr u1 fileid -- u2 ior )
void readFile() {
    auto f = (std::fstream*)Tos;
    auto len =  *Stack++;
    auto caddr = (char*) Stack[0];
    f->read(caddr, static_cast<std::streamsize>(len));
    Tos = f->bad() ? -1 : 0;
    Stack[0] = static_cast<sCell>(f->gcount());

} pp(readFile)

// READ-LINE ( c-addr u1 fileid -- u2 flag ior )
void readLine() {
    auto f = (std::fstream*)Tos;
    if (f->eof()) {
        Tos = 0;
        Stack[0] = 0;
        Stack[1] = 0;
        return;
    }
    auto length = Stack[0];
    auto caddr = (char*)(Stack[0]);
    f->getline(caddr, static_cast<std::streamsize>(length) + 1);
    if (f->bad()) {
        Tos = -1;
        Stack[0] = 0;
        Stack[1] = 0;
    }
    else if (f->eof() && std::strlen(caddr) == 0) {
        Tos = 0;
        Stack[0] = 0;
        Stack[1] = 0;
    }
    else {
        Tos = 0;
        Stack[0] = -1;
        Stack[1] = strlen(caddr);
    }
} pp(readLine)

// WRITE-FILE ( c-addr u fileid -- ior )
void writeFile() {
    auto f = (std::fstream*)Tos;
    auto len = *Stack++;
    auto caddr = (char*) *Stack++;
    f->write(caddr, static_cast<std::streamsize>(len));
    Tos = f->bad() ? -1 : 0;

} pp(writeFile)

// WRITE-LINE ( c-addr u fileid -- ior )
void writeLine() {
    auto f = (std::fstream*)Tos;
    auto len = *Stack++;
    auto caddr = (char*) *Stack++;
    f->write(caddr, static_cast<std::streamsize>(len));
    (*f) << endl;
    Tos = f->bad() ? Cell(-1) : 0;
} pp(writeLine)

// FILE-POSITION ( fileid -- ud ior ) \ 94 FILE
void FilePosition()
{    auto f = (std::fstream*)Tos;
	*--Stack = f->tellg();
	*--Stack = 0;
	Tos = 0;
}  pp(FilePosition)

// REPOSITION-FILE ( ud fileid -- ior ) \ 94 FILE
void RepositionFile()
{    auto f = (std::fstream*)Tos;
	Stack++;
	f->seekg(*Stack++, f->beg);
	Tos = 0;
}  pp(RepositionFile)

void PrintFileStep() {
    string ReturnString;
    auto f = (std::fstream *) Tos;
    if( getline( *f, ReturnString) )
    {  cout << ReturnString;
       Tos = 0;
       return;
    }  Tos = -1;
} pp(PrintFileStep)

void SourceSave()
{   TibVector.push_back(CmdString);
} pp(SourceSave)

void SourceRest()
{   CmdString = TibVector.back();
    TibVector.pop_back();
} pp(SourceRest)

void ToSource()
{  char* ch = *(char**) Stack;
   CmdString.assign(ch, ch + Tos);
   i2drop();
} pp(ToSource)

void COutRestore()
{  std::cout.rdbuf(coutbuf); } pp(COutRestore)

void DupCOutSet()
{ std::cout.rdbuf( ((fstream *) Tos)->rdbuf());
} pp(DupCOutSet)

void Allocate()
{
  int* ptr = (int*) malloc(Tos);
  if (!ptr) { Tos = -1; 
  } else  Tos = 0; 
  *--Stack = (sCell) ptr;
} pp(Allocate)

void GetARGV()
{	*--Stack= Tos;
	*--Stack=(sCell)ARGV1;
	Tos=strlen(ARGV1);
} pp(GetARGV)


void Key() {
	  char ch;
	  int n;
	  int __fd=0;
	  struct termios t1, t2;

	      tcgetattr(__fd, &t1);
	      t2 = t1;
	      t2.c_lflag &= ~ICANON;
	      t2.c_lflag &= ~ECHO;
	      t2.c_lflag |= ISIG;
	      t2.c_cc[VMIN] = 1;
	      t2.c_cc[VTIME] = 0;
	      tcsetattr(__fd, TCSANOW, &t2);

		ch = cin.get();

	      tcsetattr(__fd, TCSANOW, &t1);

  *--Stack= Tos;
	Tos =(sCell) ch;
} pp(Key)


void Halt() { exit(Tos);} pp(Halt)
void Bye() { exit(0);} pp(Bye)

const char *initScript =
        " : 2NIP 2SWAP 2DROP ;\n"
        " : COMPILE, , ;\n"
        " : HEX 16 BASE ! ;\n"
        ": DECIMAL 10 BASE ! ;\n"
        ": HEADER BUILD SMUDGE ;\n"
        ": CONSTANT HEADER DOCONST , , ;\n"
        ": CREATE HEADER DOVAR , ;\n"
        ": VARIABLE CREATE 0 , ;\n"
        ": [COMPILE] ' , ; IMMEDIATE\n"
        ": CELL+ CELL + ;\n"
        ": CELL- CELL - ;\n"
        ": CELLS CELL * ;\n"
        ": >BODY CELL+ ;\n"
        ": COMPILE R> DUP @ , CELL+ >R ;\n"
        ": CHAR  PARSE-NAME DROP C@ ;\n"
        ": [CHAR] CHAR LIT,  ; IMMEDIATE\n"
        ": [']  ' LIT, ; IMMEDIATE\n"
        ": .( [CHAR] ) PARSE TYPE ; IMMEDIATE\n"
        ": ( [CHAR] ) PARSE 2DROP ; IMMEDIATE\n"
        ": SLIT, ( string -- ) COMPILE <$> $, ;\n"
        ": \\ 10 PARSE 2DROP  ; IMMEDIATE\n"
        ": .\\ 10 PARSE TYPE cr ; IMMEDIATE\n"
        ": .\" [CHAR] \" PARSE SLIT, COMPILE TYPE   ; IMMEDIATE\n"
        ": S\" [CHAR] \" PARSE ?STATE IF SLIT, THEN ; IMMEDIATE\n"
        ": ABORT -1 THROW ;\n"
        ": POSTPONE\n" // 94
        "  PARSE-NAME SFIND DUP\n"
        "  0= IF -321 THROW THEN \n"
        "  1 = IF COMPILE,\n"
        "      ELSE LIT, ['] COMPILE, COMPILE, THEN\n"
        "; IMMEDIATE\n"
        ": TO '\n"
        "   ?STATE 0= IF >BODY ! EXIT THEN\n"
        "    >BODY LIT, POSTPONE ! ; IMMEDIATE\n"
	": ERASE 0 FILL ;\n"
	": $!\n" //	( addr len dest -- )
	"SWAP 255 AND SWAP	2DUP C! 1+ SWAP CMOVE ;\n"
        ": DEFER@  ( xt1 -- xt2 )  >BODY @ ;\n"
        ": VALUE CONSTANT ;\n"
        ": (DO)   ( n1 n2 ---)\n"
        // Runtime part of DO.
        " R> ROT ROT SWAP >R >R >R ;\n"
        ": (?DO)  ( n1 n2 ---)\n"
        // Runtime part of ?DO
        "  OVER OVER - IF R> ROT ROT SWAP >R >R CELL+ >R \n"
        "                 ELSE DROP DROP R> @ >R \\ Jump to leave address if equal\n"
        "                 THEN ;\n"
        ": I ( --- n )\n"
        // Return the counter (index) of the innermost DO LOOP
        "  POSTPONE R@ ; IMMEDIATE\n"
                ": z\\ 10 PARSE h. h. ; IMMEDIATE\n"

        ": J  ( --- n)\n"
        // Return the counter (index) of the next loop outer to the innermost DO LOOP
        " RP@ 3 CELLS + @ ;\n"
        "VARIABLE 'LEAVE ( --- a-addr)\n" // This variable is  used  for  LEAVE address resolution.

        ": (LEAVE)   ( --- )\n"
        // Runtime part of LEAVE
        " R> @ R> DROP R> DROP >R ;\n" // Remove loop parameters and replace top of ret\n"
        // stack by leave address.\n"

        ": UNLOOP ( --- )\n"
        // Remove one set of loop parameters from the return stack.
        "   R> R> DROP R> DROP >R ;\n"

        ": (LOOP) ( ---)\n"
        // Runtime part of LOOP
        "  R> R> 1+ DUP R@ = \n"   // Add 1 to count and compare to limit.
        "  IF \n"
        "   R> DROP DROP CELL+ >R\n" // Discard parameters and skip leave address.
        "  ELSE \n"
        "   >R @ >R\n" // Repush counter and jump to loop start address.
        "  THEN ;\n"

        ": (+LOOP) ( n ---)\n"
        // Runtime part of +LOOP
        // Very similar to (LOOP), but the compare condition is different.
        //  exit if ( oldcount - lim < 0) xor ( newcount - lim < 0).
        "     R> SWAP R> DUP R@ - ROT ROT + DUP R@ - ROT XOR 0 < \n"
        "     IF R> DROP DROP CELL+ >R\n"
        "     ELSE >R @ >R THEN ;\n"

        ": DO ( --- x)\n"
        // Start a DO LOOP.
        // Runtime: ( n1 n2 --- ) start a loop with initial count n2 and
        // limit n1.
        "  POSTPONE (DO) 'LEAVE @  HERE 0 'LEAVE ! \n"
        "   ; IMMEDIATE\n"

        ": ?DO  ( --- x )\n"
        // Start a ?DO LOOP.\n"
        // Runtime: ( n1 n2 --- ) start a loop with initial count n2 and
        // limit n1. Exit immediately if n1 = n2.
        "  POSTPONE (?DO)  'LEAVE @ HERE 'LEAVE ! 0 , HERE ; IMMEDIATE\n"

        ": LEAVE ( --- )\n"
        // Runtime: leave the matching DO LOOP immediately.
        // All places where a leave address for the loop is needed are in a linked\n"
        // list, starting with 'LEAVE variable, the other links in the cells where
        // the leave addresses will come.
        "  POSTPONE (LEAVE) HERE 'LEAVE @ , 'LEAVE ! ; IMMEDIATE\n"

        ": RESOLVE-LEAVE\n"
        // Resolve the references to the leave addresses of the loop.
        "         'LEAVE @\n"
        "         BEGIN DUP WHILE DUP @ HERE ROT ! REPEAT DROP ;\n"

        ": LOOP  ( x --- )\n"
        // End a DO LOOP.
        // Runtime: Add 1 to the count and if it is equal to the limit leave the loop.
        " POSTPONE (LOOP) ,  RESOLVE-LEAVE  'LEAVE ! ; IMMEDIATE\n"

        ": +LOOP  ( x --- )\n"
        // End a DO +LOOP
        // Runtime: ( n ---) Add n to the count and exit if this crosses the
        // boundary between limit-1 and limit.
        " POSTPONE (+LOOP) , RESOLVE-LEAVE 'LEAVE ! ; IMMEDIATE\n"

        ": (;CODE) ( --- )\n"
        // Runtime for DOES>, exit calling definition and make last defined word
        // execute the calling definition after (;CODE)
        "  R> LAST @  NAME>  ! ;\n"

        ": DOES>  ( --- )\n"
        // Word that contains DOES> will change the behavior of the last created
        // word such that it pushes its parameter field address onto the stack
        // and then executes whatever comes after DOES>
        " POSTPONE (;CODE) \n"
        " POSTPONE R>\n" // Compile the R> primitive, which is the first
        // instruction that the defined word performs.
        "; IMMEDIATE\n"

    ": SET-CURRENT ( wid -- )\n" // 94 SEARCH
    "        CURRENT ! ;\n"

    ": GET-CURRENT ( -- wid )\n" // 94 SEARCH
    "        CURRENT @ ;\n"
    ": GET-ORDER ( -- widn ... wid1 n )\n"  // 94 SEARCH
        " SP@ >R 0 >R\n"
        " CONTEXT\n"
        " BEGIN DUP @ ?DUP\n"
        " WHILE >R CELL+\n"
        " REPEAT  DROP\n"
        " BEGIN R> DUP 0=\n"
        " UNTIL DROP\n"
        "R> SP@ - CELL / 1- ; \n"

	" HERE S\" FORTH\" $, FORTH-WORDLIST CELL+ !\n"
        ": VOC-NAME. ( wid -- )\n"
        "DUP CELL+ @ DUP IF COUNT TYPE BL EMIT DROP ELSE DROP .\" <NONAME>:\" U. THEN ;\n"

        ": ORDER ( -- )\n" // 94 SEARCH EXT
        "GET-ORDER .\" Context: \" \n"
        "0 ?DO ( DUP .) VOC-NAME. SPACE LOOP CR\n"
        ".\" Current: \" GET-CURRENT VOC-NAME. CR ;\n"

        ": SET-ORDER ( wid1 ... widn n -- )\n"
        "DUP -1 = IF\n"
        "DROP  FORTH-WORDLIST 1\n"
        "THEN\n"
        "DUP  CONTEXT-SIZE  U> IF -49 THROW THEN\n"
        "DUP CELLS context + 0!\n"
        "0 ?DO I CELLS context + ! LOOP ;\n"
        "CREATE VOC-LIST FORTH-WORDLIST CELL+ CELL+ ,\n"

        ": FORTH FORTH-WORDLIST CONTEXT ! ;\n"
        ": DEFINITIONS  CONTEXT @ CURRENT ! ;\n"

        ": WORDLIST ( -- wid )\n" // 94 SEARCH
        " HERE 0 , 0 , \n"
        " HERE VOC-LIST  @ , .\" W=\" DUP H.  VOC-LIST ! ;\n"

	": ONLY ( -- ) -1 SET-ORDER ;\n"
	": ALSO ( -- )   GET-ORDER OVER SWAP 1+ SET-ORDER ;\n"
	": PREVIOUS ( -- ) GET-ORDER NIP 1- SET-ORDER ;\n"

 ": LATEST ( -> NFA ) CURRENT @ @ ;\n"

": VOCABULARY ( <spaces>name -- )\n"
"WORDLIST\n"
" CREATE DUP ,\n"
"LATEST SWAP CELL+ !\n"
"DOES>  @ CONTEXT ! ;\n"
 " VARIABLE CURSTR\n"
": FQUIT  BEGIN REFILL WHILE CURSTR 1+! INTERPRET REPEAT ;\n"
 ": INCLUDE-FILE\n" // ( fid --- )
// Read lines from the file identified by fid and interpret them.
// INCLUDE and EVALUATE nest in arbitrary order.
  "SOURCE-ID >R >IN @ >R LASTIN @ >R CURSTR @ >R CURSTR 0! SOURCE-SAVE\n"
  "TO SOURCE-ID\n"
  "['] FQUIT CATCH SAVEERR \n"
    "SOURCE-REST R> CURSTR ! R> LASTIN ! R> >IN ! R> TO SOURCE-ID THROW ;\n"
// " VOCABULARY wwww  LATEST count type  \n"
  "444 CONSTANT  CFNAME_SIZE\n"
  "CREATE CURFILENAME  CFNAME_SIZE 255 + 1+ ALLOT\n"
  "CURFILENAME  CFNAME_SIZE 255 + 1+  ERASE\n"
  ": CFNAME-SET\n" // ( adr len -- )
  "DUP 1+ >R  CURFILENAME CURFILENAME R@ + CFNAME_SIZE R> - CMOVE>\n"
  "CURFILENAME $! ;\n"

  ": CFNAME-FREE\n" //  ( -- )
  "CURFILENAME COUNT + CURFILENAME\n"
  "CFNAME_SIZE CURFILENAME C@ - 255 +  CMOVE ;\n"

 ": INCLUDED\n" // ( c-addr u ---- )  cr DEFINITIONS : QWER ; WORDS ORDER
// " cr  .\" i=<\" 2dup type .\" >\"   2DUP CFNAME-SET\n"
 "2DUP CFNAME-SET\n"
 "R/O OPEN-FILE IF -38 THROW THEN\n"
 "DUP >R ['] INCLUDE-FILE CATCH\n"
 "DUP IF cr .\" in <\" CURFILENAME COUNT TYPE .\" >\" THEN  CFNAME-FREE\n"
 "R> CLOSE-FILE DROP THROW ;\n"

 ": EVALUATE\n" // ( i*x c-addr u -- j*x ) \ 94
 "SOURCE-ID >R SOURCE-SAVE >IN @ >R\n"
 "-1 TO SOURCE-ID\n"
 "TO_SOURCE >IN 0!\n"
 "['] INTERPRET CATCH\n"
 "R> >IN ! SOURCE-REST R> TO SOURCE-ID\n"
 "THROW ;\n"

 ": FLOAD PARSE-NAME INCLUDED ;\n"

 ": [DEFINED]\n" //  ( -- f ) \ "name"
 "PARSE-NAME  SFIND  IF DROP -1 ELSE 2DROP 0 THEN ; IMMEDIATE\n"

 ": [UNDEFINED]\n" //  ( -- f ) \ "name"
 "POSTPONE [DEFINED] 0= ; IMMEDIATE\n"

 ": \\+	POSTPONE [UNDEFINED]	IF POSTPONE \\ THEN ; IMMEDIATE\n"
 ": \\-	POSTPONE [DEFINED]	IF POSTPONE \\ THEN ; IMMEDIATE\n"

 ": BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE\n"

 ": PRIM? 0< ['] DUP 0< = ;\n"

 ": ?CONST\n" // ( cfa -- cfa flag )
 "DUP PRIM? IF 0 BREAK\n"
 "DUP @ DOCONST = ;\n"

 ": ?VARIABLE\n" // ( cfa -- cfa flag )
 "DUP PRIM? IF 0 BREAK\n"
 "DUP @ DOVAR = ;\n"

//  "FLOAD autoexec.4\n"
 "GETARGV  EVALUATE\n"
 //" CR GETARGV h. h.\n"

//    "CR .( WORDS - type of list the definition names ) CR\n"
;

void  InitStringSet()
{    CmdString = initScript ;
    *v2in = 0;
} pp(InitStringSet)

void fstreamTest(); pp(fstreamTest)

void  MakeImag(void)
{
    FthItem("NOOP", pNoop );
    FthItem("+", pAdd );
    FthItem("-", pSub );
    FthItem("D+", pDAdd );
    FthItem("1+", pi1Add );
    FthItem("1-", pi1Sub );
    FthItem("2+", pi2Add );
    FthItem("2-", pi2Sub );
    FthItem("INVERT", pInvert);
    FthItem("NEGATE", pNegate);
    FthItem("DNEGATE", pDNegate);
    FthItem("DABS", pDAbs);
    FthItem("*", pMul);
    FthItem("/", pDiv);
    FthItem("2*", pi2Mul);
    FthItem("2/", pi2Div);
    FthItem("MOD", pMod);
    FthItem("U*", pUMul);
    FthItem("U/", pUDiv);
    FthItem("UM*", pUMMul);
    FthItem("UM/MOD", pUMMOD);
    FthItem("AND", pAnd);
    FthItem("AND", pAnd);
    FthItem("OR", pOr);
    FthItem("XOR", pXor);
    FthItem("ARSHIFT", pARshift);
    FthItem("RSHIFT", pRshift);
    FthItem("LSHIFT", pLshift);
    FthItem("DUP", pDup );
    FthItem("CS-DUP", pDup );
    FthItem("?DUP", pQDup );
    FthItem("OVER", pOver );
    FthItem("CS-OVER", pOver );
    FthItem("TUCK", pTuck );
    FthItem("PICK", pPick );
    FthItem("CS-PICK", pPick );
    FthItem("SWAP", pSwap );
    FthItem("CS-SWAP", pSwap );
    FthItem("2SWAP", pi2Swap );
    FthItem("ROT", pRot );
    FthItem("DROP", pDrop );
    FthItem("NIP", pNip );
    FthItem("2DROP", pi2drop );
    FthItem("2DUP", pi2dup );
    FthItem("2OVER", pi2over);
    FthItem(".",pDot);
    FthItem("U.",pUDot);
    FthItem("H.",pHDot);
    FthItem("CATCH",(sCell)pCatch);
    FthItem("THROW",pFThrow);
    FthItem("[", pIMode); Immediate();
    FthItem("]", pCMode);
    FthItem("@", pLoad);
    FthItem("C@", pCLoad);
    FthItem("C!", pCStore);
    FthItem("C!A", pCStoreA);
    FthItem("W@", pWLoad);
    FthItem("W!", pWStore);
    FthItem("L@", pLLoad);
    FthItem("L!", pLStore);
    FthItem("2!", pi2Store);
    FthItem("2@", pi2Load);
    FthItem("COUNT", pCount);
    FthItem("!", pStore);
    FthItem("+!", pAddStore);
    FthItem("1+!", pIncr);
    FthItem("0!", pOff);
    FthItem("OFF", pOff);
    FthItem("ON", pOn);
    FthItem("=", pEqual);
    FthItem("<>", pNEqual);
    FthItem("0<", pZLess);
    FthItem("0=", pZEqual);
    FthItem("0<>", pZNEqual);
    FthItem("D0=", pDZEqual);
    FthItem("<", pLess);
    FthItem(">", pGreat);
    FthItem("U<", pULess);
    FthItem("U>", pUGreat);
    FthItem("MAX", pMax);
    FthItem("MIN", pMin);
    FthItem("0MAX", pi0Max);
    FthItem(">R", pToR);
    FthItem("R>", pFromR);
    FthItem("RDROP", pRDrop);
    FthItem("R@", pRLoad);
    FthItem("2>R", pi2ToR);
    FthItem("2R>", pi2FromR);
    FthItem("2R@", pi2RLoad);
    FthItem("RP@", pRLoad);
    FthItem("RP@", pRPGet);
    FthItem("SP@", pSPGet);
    FthItem("RP!", pRPSet);
    FthItem("SP!", pSPSet);
    FthItem(",", pCompile);
    FthItem("ALLOT", pAllot);
    FthItem("$,", pStrCmp);
    FthItem("<$>", pStr);
    FthItem("EXECUTE", pExecute);
    FthItem("SMUDGE", pSmudge);
    FthItem("TYPE", pType);
    FthItem("CR", pCr);
    FthItem("SPACE", pSpace);
    FthItem("EMIT", pEmit);
    FthItem(">IN", pi2in);
    FthItem("PARSE-NAME", pParseName);
    FthItem("PARSE", pParse);
    FthItem("SHEADER", pSHeader);
    FthItem("BUILD", pBuild);
    FthItem("SFIND", pSFind);
    FthItem("SEARCH-WORDLIST", pSearchWordList);
    FthItem("COMPARE", pCompare);
    FthItem("UCOMPARE", pUCompare);
    FthItem("FILL", pFill);
    FthItem("CMOVE", pCmove);
    FthItem("CMOVE>", pCmove_up);

    FthItem("IMMEDIATE", pImmediate);
    FthItem(":", pColon);
    FthItem(";", pSemicolon);   Immediate();
    FthItem("IF", pIf);         Immediate();
    FthItem("ELSE", pElse);     Immediate();
    FthItem("THEN", pThen);     Immediate();
    FthItem("BEGIN", pBegin);   Immediate();
    FthItem("UNTIL", pUntil);   Immediate();
    FthItem("AGAIN", pAgain);   Immediate();
    FthItem("WHILE", pWhile);   Immediate();
    FthItem("REPEAT", pRepeat); Immediate();

    FthItem("EXIT", pExit );
    Constant("STATE",(sCell) &State );
    FthItem("?STATE",pStateQ);

    Constant("DOVAR",pDoVar );
    Constant("DOCONST",pDoConst );
    Constant("DODEFER",pDoDefer );
    Constant("DP", (sCell)&here );
    Constant("LAST", (sCell)&Last );
    Constant("LASTCFA", (sCell)&LastCFA );
    Variable("WARNING",-1);
    FthItem("HERE",pHere);
    Constant("BL",(sCell)' ' );
    sCell pCell = Constant("CELL",sizeof(Cell) );
    FthItem("NAME>",pFromName);
    Constant("BASE",(sCell)&numericBase);

    Header("'");   Co(pParseName,pSFind, pZEqual,pFThrow,pExit);

    Constant("STATE",(sCell) &State );
    sCell pHi = Header("HI"); Tp("Hello!!!"); Co(pExit);
    sCell pLastin = Constant("LASTIN", (sCell)&Lastin );
    sCell pSaveErrQ = Constant("SAVE-ERR?", (sCell)&SaveErrQ );
    FthItem("SAVEERR", pSaveError);
    FthItem("PRINTERR", pPrintErr);

    sCell pContext = Constant("CONTEXT",(sCell) &Context );
    Constant("CURRENT",(sCell) &Current );
    Constant("IMAGE-BEGIN",(sCell)HereArea );
    Constant("FORTH-WORDLIST",(sCell) &ForthWordlist );
    Constant("CONTEXT-SIZE",ContextSize );
    sCell pSP0 = Variable("SP0",(sCell) &StackArea[STACK_SIZE-9] );

    FthItem("R/O",preadOnly);
    FthItem("R/W",preadWrite);
    FthItem("W/O",pwriteOnly);
    FthItem("W/O",pwriteOnly);
    FthItem("BIN",pbin);
    FthItem("CREATE-FILE",pcreateFile);
    FthItem("OPEN-FILE",popenFile);
    FthItem("READ-FILE",preadFile);
    FthItem("WRITE-FILE",pwriteFile);
    FthItem("READ-LINE",preadLine);
    FthItem("FILE-POSITION",pFilePosition);
    FthItem("REPOSITION-FILE",pRepositionFile);

    FthItem("CLOSE-FILE",pcloseFile);
    FthItem("DUPCOUTSET",pDupCOutSet);
    FthItem("COUTRESTORE",pCOutRestore);
    FthItem("FSTST",pfstreamTest);

//    FthItem("TIB",pTib);
//    FthItem("#TIB@",pNTib);

    FthItem("SOURCE",pSource);
    FthItem("SOURCE-ID",pSourceId);
    FthItem("SOURCE-SAVE",pSourceSave);
    FthItem("SOURCE-REST",pSourceRest);
    FthItem("TO_SOURCE",pToSource);
    FthItem("GETARGV",pGetARGV);
    FthItem("ALLOCATE",pAllocate);
    FthItem("KEY",pKey);
    FthItem("HALT",pHalt);
    FthItem("BYE",pBye);

    Constant("YDP", (sCell)&YDP);
    Constant("YDP0", (sCell)&YDP0);
    FthItem("YDP_FL",pYDPFL);

    sCell pLitC = Header("LIT,");  Co(pDoDefer,pLitCo);
    sCell pPre = Header("<PRE>");  Co(pDoDefer,pNoop);
    sCell pQStack = Header("?STACK");  Co(pDoDefer,pNoop);

    sCell pRefill = Header("REFILL");
	Co(pSourceId);
    If();  Co(pFileRefill,pDup); If(); Co(pPre); Then();
    Else(); Co(pAskE);
    Then(); Co(pExit);


    Header("PRINT-FILE");
    Begin();  Co( pDup,  pPrintFileStep);
    Until();  Co( pDrop,pExit);

    FthItem("SNUMBER0",pSNunber0);

    sCell pSNunber = Header("SNUMBER");
    Co(pDoDefer,pSNunber0 );

    sCell pQSLiteral0 = Header("?SLITERAL0");
	Co(pSNunber );
	If(); Lit(-321); Co(pFThrow);
	Else(); Co( pStateQ); If(); Co(pLitC); Then();
	Then();
    Co(pExit);

    sCell pQSLiteral = Header("?SLITERAL");
    Co(pDoDefer,pQSLiteral0);

    sCell pInterpret1 = Header("INTERPRET1");
    Begin();
        Co( pi2in, pLoad , pLastin, pStore , pSaveErrQ, pOn);
        Co( pParseName, pDup );
    While();  Co(pSFind, pQDup );
        If();
            Co(   pStateQ, pEqual);
            If();   Co( pCompile );
            Else(); Co( pExecute );
            Then();
        Else();   Co(pQSLiteral);
        Then();  Co(pQStack);
    Repeat();
    Co(pi2drop,pExit );

    sCell pInterpret = Header("INTERPRET");
    Co(pDoDefer,pInterpret1 );

    sCell pQuit = Header("QUIT");
    Begin();    Co(pRefill );
    While();   Co(pInterpret );  Tp(" ok\r\n");
    Repeat();
    Co(pExit );

    Header("WORDS");
    Co(pContext, pLoad, pLoad);
    Begin(); Co(pDup);
    While(); Co(pDup, pCount, pType, pSpace, pCell, pSub, pLoad );
    Repeat();  Co(pDrop, pExit );

    ip = here;  // SYS START
    Tp("CppForth\r\n");

    Co( pInitStringSet, pIMode,  pLit, pInterpret,pCatch, pQDup );
    If(); Co(pSaveError,pPrintErr, pSP0,pLoad, pSPSet,pCr ) ;
    Then();

    Begin();
        Co(  pIMode,  pLit, pQuit,pCatch, pSaveError );
        Co( pPrintErr, pSP0, pLoad, pSPSet,pCr ) ;
    Again();

} pp(MakeImag)

void fstreamTest()    // FSTST
{
    auto caddr = (char*)(Stack[1]);
    auto len = Stack[0];
    auto fam = static_cast<std::ios_base::openmode>(Tos);
    Tos = *Stack++;
    auto fl = new std::fstream( string(caddr, len) , fam | std::ios_base::trunc);

    if (fl->is_open()) {
        Stack[0] =(sCell)fl;
        Tos = 0;
    }
    else {
        delete fl;
        Stack[0] = 0;
        Tos = -1;
    }

//fstream * fl = new std::fstream( "CoolNumberqq.txt" , std::ios_base::out );
  Drop();
 std::cout.rdbuf( ((fstream *) Tos)->rdbuf());

// std::cout.rdbuf(fl->rdbuf());

cout<<"1234567890";
cout<<"1234567890";
cout<<"1234567890";
cout<<"1234567890";

COutRestore();

fl->close();

}

main(int argc,char **argv){
   ARGV1=argv[1];
 if(!ARGV1) ARGV1=string("").data();
  printf("\n<%s>\n", ARGV1) ;
 coutbuf = cout.rdbuf(); 
//  fstreamTest();

  if(pNoop<0){
      // positiv addresses area
      for (;;)
      {   do{
              ((proc) (~ireg) )();
              ireg = *ip++;
          }while ( ireg<0);
          do{
              *--rStack = (sCell) ip;  ip =  (sCell *) ireg;
              ireg = *ip++;
          }while ( ireg>0);
      }
  }
  else{
      // negative addresses area
        for (;;)
        {   do{
                ((proc) (~ireg) )();
                ireg = *ip++;
            }while ( ireg>0);
            do{
                *--rStack = (sCell) ip;  ip =  (sCell *) ireg;
                ireg = *ip++;
            }while ( ireg<0);
        }
    }
}
