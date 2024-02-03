
int gettos(long long);

unsigned ttt( unsigned uu)
{ ramf();
 return uu/4;
}

int tt1( int uu)
{ return uu|5;
}

long long getltos();
long long  * getlpsp();

long long ccl()
{ long long *psp;
 long long  ttos,tmp;

  ttos=getltos();
  psp=getlpsp();
if(gettos(ttos))  ttos+=*psp++;    // add
if(gettos(ttos))     // swap
 {   long long tt = *psp ;
  *psp=ttos;
   ttos=tt;
}
if(gettos(ttos))  ttos-=*psp++;    // sub
if(gettos(ttos))  ttos=-ttos;    // negate
if(gettos(ttos))  ttos-=*psp++;    // sub

  return ttos;
}
