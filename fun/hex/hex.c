#include <stdio.h>

#define BYTE unsigned char
#define WORD unsigned int


//===========================================================================
char* hexstring(char* d,BYTE* s,int n,char stuf)    //dest must have 71 chars
{
#define BLANK \
  "                                                    |                |"
#define DOTOFF   53      //offset of first dot
  static WORD offs[]={0,3,6,9, 13,16,19,22, 26,29,32,35, 39,42,45,48};
  static BYTE htab[]="0123456789ABCDEF";
  static char dest[80];
  BYTE c;
  int i;

  if (d==NULL) d=dest;                 //use my own space if told to
  strcpy(d,BLANK);
  if ((n<0)||(n>16)) n=16;
  for (i=0;i<n;i++) {
    c=s[i];
    d[offs[i]]=htab[c>>4];
    d[offs[i]+1]=htab[c&0x0F];
    if (!stuf || isprint(c)) d[DOTOFF+i]=c; else d[DOTOFF+i]=stuf;
  }   //for
  return d;
}   //hexstring


int  main(int argc,char* argv[])
{
  unsigned char b[16];
  FILE* f;
  long n=0;
  int k;

  if (argc!=2) { printf("hexdump <filename>\n"); return 1; }
  f=fopen(argv[1],"rb");
  if (!f) { printf("file \"%s\" wouldn't open.\n",argv[1]); return 2; }
  printf("Hexdump of \"%s\"...\n",argv[1]);
  while ((k=fread(b,1,16,f))!=0) {
    printf("%06lX: %s\n",n,hexstring(NULL,b,k,' '));
    n += (long)k;
  };
  printf("0x%06lX (%lu) bytes read.\n",n,n);
  fclose(f);
  return 0;
}   //main

