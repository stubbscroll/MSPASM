#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <ctype.h>

#define MAXSYM 1000      /* Maximum size of symbol table */
#define MAXLEX 10000     /* Maximum size of lexemes array */
#define MAXBUF 200       /* Maximum size for one lexeme */

char *infilename;        /* Source file name */
char *outfilename;       /* Object file name */

unsigned char *infile;   /* Source file */
unsigned long infilelen; /* File length */
int infilepos;           /* Position in file */
unsigned char *outfile;  /* Area for output file */
long minpos;             /* Earliest used written to pos */
long maxpos;             /* Max used pos in output file */

int lineno;              /* Current line number */

int accusize;            /* Size of accumulator, 0=8-bit, 1=16-bit */
int indexsize;           /* Size of x/y-index, 0=8-bit, 1=16-bit */

/* Options */

int opt_c=1;             /* Processor: 1=6502, 2=6510, 3=65816 */

void error(char *s)
{
   fprintf(stderr,"%s",s);
   fprintf(stderr,"error in line %d",lineno);
   exit(0);
}

void header()
{
   printf("MSP-assembler v0.01 by Ruben Spaans in April 2002\n\n");
}

void usage()
{
   printf("MSPASM -options sourcefile outfile\n");

}

void parsecommandline(int argc,char **argv)
{
   if(argc==1) {
      usage();
      exit(0);
   }

   while(argc>1 && argv[1][0]=='-') {
      if(!strcmp(argv[1],"c1"))
         opt_c=1;
      else if(!strcmp(argv[1],"c2"))
         opt_c=2;
      else if(!strcmp(argv[1],"c3"))
         opt_c=3;
      argv++;
      argc--;
   }
   if(argc==3) {
      if((infilename=(char *)malloc(strlen(argv[1])+1))==NULL)
         error("Not enough memory to hold file name.\n");
      strcpy(infilename,argv[1]);
      if((outfilename=(char *)malloc(strlen(argv[2])+1))==NULL)
         error("Not enough memory to hold file name.\n");
      strcpy(outfilename,argv[2]);
   } else error("Wrong number of arguments.\n");
}

/* ---------------
   Load input file
   --------------- */

/* Load a file into memory, return pointer to file in memory, NULL if error.
   First 4 bytes in memory buffer are length, then the file follows */

unsigned char *loadfile(char *s)
{
   unsigned char *buf;
   FILE *f;
   unsigned long l;

   f=fopen(s,"rb");
   if(!f)
      return NULL;

   l=filelength(fileno(f));
   buf=(unsigned char *)malloc(l+4);
   if(!buf) {
      fclose(f);
      return NULL;
   }
   if(l!=fread(buf+4,1,l,f)) {
      fclose(f);
      free(buf);
      return NULL;
   }
   buf[0]=l & 255;
   buf[1]=(l >> 8) & 255;
   buf[2]=(l >> 16) & 255;
   buf[3]=l >> 24;
   fclose(f);
   return buf;
}

/* Frees a previously loaded file from memory */

void unloadfile(unsigned char *buf)
{
   free(buf);
}

/* Make this support more input files later */

void loadinputfiles()
{
   infile=loadfile(infilename);
   infilelen=infile[0]+(unsigned long)infile[1]*256+(unsigned long)
      infile[2]*65536+(unsigned long)infile[3]*16777216;
}

/* Make later, when macro expansion is desired */

void preprocess()
{
}

/* --------------------------
   Routines for the assembler
   -------------------------- */

#define ASM_MNEMONIC 257
#define ASM_A 258           /* Accumulator */
#define ASM_S 259
#define ASM_X 260
#define ASM_Y 261
#define ASM_TEXT 262        /* Text */
#define ASM_BYTE 263        /* Byte */
#define ASM_WORD 264        /* Word */
#define ASM_EQU 265         /* Equ */
#define ASM_PROCESSOR 266
#define STR 297
#define ID 298
#define NUM 299
#define DONE 300

/* Addressing modes

 0 = implied                   RTS
 1 = immediate                 LDA #byte
 2 = direct page               LDA byte
 3 = absolute                  LDA word
 4 = direct page,x             LDA byte,x
 5 = absolute,x                LDA word,x
 6 = absolute,y                LDA word,y
 7 = direct (indirect,x)       LDA (byte,x)
 8 = direct (indirect),y       LDA (byte),y
 9 = absolute indirect         JMP (word)
10 = accumulator               LSR A
11 = relative                  BNE word (byte)
14 = direct page,y             LDX byte,y

SuperCPU:

12 = immediate long            LDA #word
13 = relative long             BNE word (word)
15 = direct indirect           LDA (byte)
16 = direct indirect long      LDA [byte]
17 = direct indirect long,y    LDA [byte],y
18 = absolute long             LDA dword
19 = absolute long,x           LDA dword,x
20 = stack relative            LDA byte,s
21 = stack relative indirect,y LDA (byte,s),y
22 = absolute indirect,x       LDA (word,x)
23 = absolute indirect long    JMP [word]
24 = block move                MVN byte,byte

*/

struct opcodes {
   char *mne;  /* Mnemonic */
   int addr;   /* Addressing mode */
   unsigned char b;     /* Opcode */
} mne6502[]={
   {"brk",0,0x00},
   {"ora",7,0x01},
   {"ora",2,0x05},
   {"asl",2,0x06},
   {"php",0,0x08},
   {"ora",1,0x09},
   {"asl",10,0x0A},
   {"ora",3,0x0D},
   {"asl",3,0x0E},
   {"bpl",11,0x10},
   {"ora",8,0x11},
   {"ora",4,0x15},
   {"asl",4,0x16},
   {"clc",0,0x18},
   {"ora",6,0x19},
   {"ora",5,0x1D},
   {"asl",5,0x1E},
   {"jsr",3,0x20},
   {"and",7,0x21},
   {"bit",2,0x24},
   {"and",2,0x25},
   {"rol",2,0x26},
   {"plp",0,0x28},
   {"and",1,0x29},
   {"rol",10,0x2A},
   {"and",3,0x2D},
   {"rol",3,0x2E},
   {"bmi",11,0x30},
   {"and",8,0x31},
   {"bit",3,0x34},
   {"and",4,0x35},
   {"rol",4,0x36},
   {"sec",0,0x38},
   {"and",6,0x39},
   {"and",5,0x3D},
   {"rol",5,0x3E},
   {"rti",0,0x40},
   {"eor",7,0x41},
   {"eor",2,0x45},
   {"lsr",2,0x46},
   {"pha",0,0x48},
   {"eor",1,0x49},
   {"lsr",10,0x4A},
   {"jmp",3,0x4C},
   {"eor",3,0x4D},
   {"lsr",3,0x4E},
   {"bvc",11,0x50},
   {"eor",8,0x51},
   {"eor",4,0x55},
   {"lsr",4,0x56},
   {"cli",0,0x58},
   {"eor",6,0x59},
   {"eor",5,0x5D},
   {"lsr",5,0x5E},
   {"rts",0,0x60},
   {"adc",7,0x61},
   {"adc",2,0x65},
   {"ror",2,0x66},
   {"pla",0,0x68},
   {"adc",1,0x69},
   {"ror",10,0x6A},
   {"jmp",9,0x6C},
   {"adc",3,0x6D},
   {"rol",3,0x6E},
   {"bvs",11,0x70},
   {"adc",8,0x71},
   {"adc",4,0x75},
   {"ror",4,0x76},
   {"sei",0,0x78},
   {"adc",6,0x79},
   {"adc",5,0x7D},
   {"ror",5,0x7E},
   {"sta",7,0x81},
   {"sty",2,0x84},
   {"sta",2,0x85},
   {"stx",2,0x86},
   {"dey",0,0x88},
   {"txa",0,0x8A},
   {"sty",3,0x8C},
   {"sta",3,0x8D},
   {"stx",3,0x8E},
   {"bcc",11,0x90},
   {"sta",8,0x91},
   {"sty",4,0x94},
   {"sta",4,0x95},
   {"stx",14,0x96},
   {"tya",0,0x98},
   {"sta",6,0x99},
   {"txs",0,0x9A},
   {"sta",5,0x9D},
   {"ldy",1,0xA0},
   {"lda",7,0xA1},
   {"ldx",1,0xA2},
   {"ldy",2,0xA4},
   {"lda",2,0xA5},
   {"ldx",2,0xA6},
   {"tay",0,0xA8},
   {"lda",1,0xA9},
   {"tax",0,0xAA},
   {"ldy",3,0xAC},
   {"lda",3,0xAD},
   {"ldx",3,0xAE},
   {"bcs",11,0xB0},
   {"lda",8,0xB1},
   {"ldy",4,0xB4},
   {"lda",4,0xB5},
   {"ldx",14,0xB6},
   {"clv",0,0xB8},
   {"lda",6,0xB9},
   {"tsx",0,0xBA},
   {"ldy",5,0xBC},
   {"lda",5,0xBD},
   {"ldx",6,0xBE},
   {"cpy",1,0xC0},
   {"cmp",7,0xC1},
   {"cpy",2,0xC4},
   {"cmp",2,0xC5},
   {"dec",2,0xC6},
   {"iny",0,0xC8},
   {"cmp",1,0xC9},
   {"dex",0,0xCA},
   {"cmp",3,0xCD},
   {"dec",3,0xCE},
   {"bne",11,0xD0},
   {"cmp",8,0xD1},
   {"cpy",3,0xD4},
   {"cmp",4,0xD5},
   {"dec",4,0xD6},
   {"cld",0,0xD8},
   {"cmp",6,0xD9},
   {"cmp",5,0xDD},
   {"dec",5,0xDE},
   {"cpx",1,0xE0},
   {"sbc",7,0xE1},
   {"cpx",2,0xE4},
   {"sbc",2,0xE5},
   {"dec",2,0xE6},
   {"inx",0,0xE8},
   {"sbc",1,0xE9},
   {"nop",0,0xEA},
   {"cpx",3,0xEC},
   {"sbc",3,0xED},
   {"inc",3,0xEE},
   {"beq",11,0xF0},
   {"sbc",8,0xF1},
   {"sbc",4,0xF5},
   {"inc",4,0xF6},
   {"sed",0,0xF8},
   {"sbc",6,0xF9},
   {"sbc",5,0xFD},
   {"inc",5,0xFE},
   {"",255,0}
};


struct opcodes mne6510[]={
   {"slo",7,0x03},
   {"slo",2,0x07},
   {"anc",1,0x0B},
   {"no0",3,0x0C},

   {"isb",5,0xFF},
   {"",255,0}
};

struct opcodes mne65816[]={
   {"cop",0,0x02},
   {"ora",20,0x03},
   {"tsb",2,0x04},
   {"ora",16,0x07},
   {"ora",12,0x09},
   {"phd",0,0x0B},
   {"tsb",3,0x0C},
   {"ora",18,0x0F},
   {"ora",15,0x12},
   {"ora",21,0x13},
   {"trb",2,0x14},
   {"ora",17,0x17},
   {"inc",10,0x1A},
   {"ina",0,0x1A},
   {"tcs",0,0x1B},
   {"tas",0,0x1B},
   {"trb",3,0x1C},
   {"ora",19,0x1F},
   {"jsr",18,0x22},
   {"jsl",18,0x22},
   {"and",20,0x23},
   {"and",16,0x27},
   {"and",12,0x29},
   {"pld",0,0x2B},
   {"and",18,0x2F},
   {"and",15,0x32},
   {"and",21,0x33},
   {"bit",5,0x34},
   {"and",17,0x37},
   {"dec",10,0x3A},
   {"dea",0,0x3A},
   {"tsc",0,0x3B},
   {"tsa",0,0x3B},
   {"bit",4,0x3C},
   {"and",19,0x3F},
   {"wdm",2,0x42},
   {"eor",20,0x43},
   {"mvp",24,0x44},
   {"eor",16,0x47},
   {"eor",12,0x49},
   {"phk",0,0x4B},
   {"eor",18,0x4F},
   {"eor",15,0x52},
   {"eor",21,0x53},
   {"mvn",24,0x54},
   {"eor",17,0x57},
   {"phy",0,0x5A},
   {"tcd",0,0x5B},
   {"tad",0,0x5B},
   {"jmp",18,0x5C},
   {"jml",18,0x5C},
   {"eor",19,0x5F},
   {"per",13,0x62},
   {"adc",20,0x63},
   {"stz",2,0x64},
   {"adc",16,0x67},
   {"adc",12,0x69},
   {"rtl",0,0x6B},
   {"adc",18,0x6F},
   {"adc",15,0x72},
   {"adc",21,0x73},
   {"stz",4,0x74},
   {"adc",17,0x77},
   {"ply",0,0x7A},
   {"tdc",0,0x7B},
   {"tda",0,0x7B},
   {"jmp",22,0x7C},
   {"adc",19,0x7F},
   {"bra",11,0x80},
   {"brl",13,0x82},
   {"sta",20,0x83},
   {"sta",16,0x87},
   {"bit",1,0x89},
   {"bit",12,0x89},
   {"phb",0,0x8B},
   {"sta",18,0x8F},
   {"blt",11,0x90},
   {"sta",15,0x92},
   {"sta",21,0x93},
   {"sta",17,0x97},
   {"txy",0,0x9B},
   {"stz",3,0x9C},
   {"stz",5,0x9E},
   {"sta",19,0x9F},
   {"ldy",12,0xA0},
   {"ldx",12,0xA2},
   {"lda",20,0xA3},
   {"lda",16,0xA7},
   {"lda",12,0xA9},
   {"plb",0,0xAB},
   {"lda",18,0xAF},
   {"bge",11,0xB0},
   {"lda",15,0xB2},
   {"lda",21,0xB3},
   {"lda",17,0xB7},
   {"tyx",0,0xBB},
   {"lda",19,0xBF},
   {"rep",1,0xC2},
   {"cmp",20,0xC3},
   {"cmp",16,0xC7},
   {"cmp",12,0xC9},
   {"wai",0,0xCB},
   {"cmp",18,0xCF},
   {"cmp",15,0xD2},
   {"cmp",21,0xD3},
   {"pei",15,0xD4},
   {"cmp",17,0xD7},
   {"phx",0,0xDA},
   {"stp",0,0xDB},
   {"jmp",23,0xDC},
   {"jml",23,0xDC},
   {"cmp",19,0xDF},
   {"sep",1,0xE2},
   {"sbc",20,0xE3},
   {"sbc",16,0xE7},
   {"sbc",12,0xE9},
   {"xba",0,0xEB},
   {"swa",0,0xEB},
   {"sbc",18,0xEF},
   {"sbc",15,0xF2},
   {"sbc",21,0xF3},
   {"pea",3,0xF4},
   {"sbc",17,0xF7},
   {"plx",0,0xFA},
   {"xce",0,0xFB},
   {"jsr",22,0xFC},
   {"sbc",19,0xFF},
   {"",255,0}
};

struct entry {
   char *lexptr;
   int token;
   long value;
} symtable[MAXSYM];     /* Symbol table */
int lastentry=0;
char lexemes[MAXLEX];   /* Lexemes */
int lastchar=-1;        /* Last used position in lexemes */
unsigned long tokenval; /* Attached token value */
int lookahead;          /* Lookahead character in lexscan */
char lexbuf[MAXBUF];    /* Lexbuffer for lexscan */

unsigned long programcounter=4096; /* Program counter */
int pass;               /* Assembly pass */
int type;               /* 0=auto, 1=8-bit 2=force 16-bit, 3=force 24-bit */

int lookupsym(char *s)
{
   int p;

   for(p=lastentry;p;p--)
      if(!strcmp(symtable[p].lexptr,s))
         return p;
   return 0;
}

int insertsym(char *s, int tok)
{
   int len;

   len=strlen(s);
   if(lastentry+1>=MAXSYM)
      error("Symbol table is full.\n");
   if(lastchar+len+1>MAXLEX)
      error("Lexemes array full.\n");
   lastentry++;
   symtable[lastentry].token=tok;
   symtable[lastentry].lexptr=&lexemes[lastchar+1];
   symtable[lastentry].value=5555;
   lastchar+=len+1;
   strcpy(symtable[lastentry].lexptr,s);
   return lastentry;
}

void initsymtable()
{
   int i;

   i=0;
   while(mne6502[i].addr<255) {
      if(!lookupsym(mne6502[i].mne))
         insertsym(mne6502[i].mne,ASM_MNEMONIC);
      i++;
   }
   i=0;
   while(mne6510[i].addr<255) {
      if(!lookupsym(mne6510[i].mne))
         insertsym(mne6510[i].mne,ASM_MNEMONIC);
      i++;
   }
   i=0;
   while(mne65816[i].addr<255) {
      if(!lookupsym(mne65816[i].mne))
         insertsym(mne65816[i].mne,ASM_MNEMONIC);
      i++;
   }
   insertsym("a",ASM_A);
   insertsym("s",ASM_S);
   insertsym("x",ASM_X);
   insertsym("y",ASM_Y);
   insertsym("text",ASM_TEXT);
   insertsym("byte",ASM_BYTE);
   insertsym("word",ASM_WORD);
   insertsym("equ",ASM_EQU);
   insertsym("processor",ASM_PROCESSOR);
}

int getnextchar()
{
   if((unsigned long)infilepos==infilelen)
      return EOF;
   return infile[infilepos+++4];
}

void ungetnextchar()
{
   if(infilepos>0)
      infilepos--;
}

/* Lexical scanner */

int lexan()
{
   int t;

   while(1) {
      t=getnextchar();
      if(t==' ' || t=='\t' || t=='\r')   /* Remove white space */
         ;
      else if(t=='\n') {
         lineno++;
         return t;
      }
      else if(isdigit(t)) {
         int b=0;
         while(isdigit(t)) {
            lexbuf[b]=t;
            t=getnextchar();
            b++;
            if(b>MAXBUF)
               error("Max lexbuffer size exceeded.\n");
         }
         lexbuf[b]=0;
         if(t!=EOF)
            ungetnextchar();
         tokenval=strtol(lexbuf,NULL,10);
         return NUM;
      } else if(t=='$') {
         int b=0;
         t=getnextchar();
         while(isdigit(t) || (toupper(t)>='A' && toupper(t)<='F')) {
            lexbuf[b]=t;
            t=getnextchar();
            b++;
            if(b>MAXBUF)
               error("Max lexbuffer size exceeded.\n");
         }
         lexbuf[b]=0;
         if(t!=EOF)
            ungetnextchar();
         tokenval=strtol(lexbuf,NULL,16);
         return NUM;
      } else if(t=='%') {
         int b=0;
         t=getnextchar();
         while(isdigit(t) && t<'2') {
            lexbuf[b]=t;
            t=getnextchar();
            b++;
            if(b>MAXBUF)
               error("Max lexbuffer size exceeded.\n");
         }
         lexbuf[b]=0;
         if(t!=EOF)
            ungetnextchar();
         tokenval=strtol(lexbuf,NULL,2);
         return NUM;
      } else if(isalpha(t) || t=='_') {
         int p,b=0;
         while(isalnum(t) || t=='_') {
            lexbuf[b]=t;
            t=getnextchar();
            b++;
            if(b>=MAXBUF)
               error("Max lexbuffer size exceeded.\n");
         }
         lexbuf[b]=0;
         if(t!=EOF)
            ungetnextchar();
         p=lookupsym(lexbuf);
         if(!p)
            p=insertsym(lexbuf,ID);
         tokenval=p;
         return symtable[p].token;
      } else if(t=='"') {
         int b=0;
         t=getnextchar();
         while(t!='"' && t!=EOF) {
            lexbuf[b]=t;
            t=getnextchar();
            b++;
            if(b>=MAXBUF)
               error("Max lexbuffer size exceeded.\n");
         }
         lexbuf[b]=0;
         return STR;
      } else if(t=='\'') {
         int b=0;
         t=getnextchar();
         while(t!='\'' && t!=EOF) {
            lexbuf[b]=t;
            t=getnextchar();
            b++;
            if(b>=MAXBUF)
               error("Max lexbuffer size exceeded.\n");
         }
         lexbuf[b]=0;
         return STR;
      } else if(t==EOF)
         return DONE;
      else {
         tokenval=0;
         return t;
      }
   }
}

void match(int t)
{
   if(lookahead==t)
      lookahead=lexan();
   else
      error("Syntax error.\n");
}

void modifypc()
{
   match('*');
   match('=');
   programcounter=tokenval;
   match(NUM);
}

/* The horribly difficult routine for evaluating expressions */

unsigned long expr();

unsigned long factor2()
{
   unsigned long val;

   if(lookahead=='(') {
      match('(');
      val=expr();
      match(')');
   } else if(lookahead==ID) {
      int p;
      p=lookupsym(lexbuf);
      if(p==0)
         val=0;
      else
         val=symtable[p].value;
      match(ID);
   } else if(lookahead==NUM) {
      val=tokenval;
      match(NUM);
   } else if(lookahead=='*') {
      val=programcounter;
      match('*');
   } else if(lookahead==STR) {
      val=(unsigned char)lexbuf[0];
      match(STR);
   } else
      error("Value or new expression expected.\n");
   return val;
}

unsigned long factor()
{
   unsigned long val;

   if(lookahead=='>') {             /* Hi-byte */
      match('>');
      val=(factor2() & 0xFF00) >> 8;
   } else if(lookahead=='<') {      /* Lo-byte */
      match('<');
      val=factor2() & 0xFF;
   } else if(lookahead=='!') {      /* Force 16-bit */
      match('!');
      val=factor2() & 0xFFFF;
      type=2;
   } else if(lookahead=='@') {      /* Force 24-bit */
      match('@');
      val=factor2();
      type=3;
   } else if(lookahead=='^') {      /* Get upper 8 bits of 24-bit value */
      match('^');
      val=(factor2() & 0xFF0000) >> 16;
   } else
      val=factor2();
   return val;
}

unsigned long morefactors(unsigned long val)
{
   int val2;

   if(lookahead=='*') {
      match('*');
      val2=factor();
      val=val*val2;
      val=morefactors(val);
   } else if(lookahead=='/') {
      match('/');
      val2=factor();
      val=val/val2;
      val=morefactors(val);
   } else if(lookahead=='%') {
      match('%');
      val2=factor();
      val=val%val2;
      val=morefactors(val);
   } else if(lookahead=='&' || (lookahead==ASM_MNEMONIC &&
      !strcmp(lexbuf,"and"))) {
      lookahead=lexan();
      val2=factor();
      val=val&val2;
      val=morefactors(val);
   } else if(lookahead=='|' || (lookahead==ID && !strcmp(lexbuf,"or"))) {
      lookahead=lexan();
      val2=factor();
      val=val|val2;
      val=morefactors(val);
   }
   return val;
}

unsigned long term()
{
   unsigned long val;

   val=factor();
   val=morefactors(val);
   return val;
}

unsigned long moreterms(val)
{
   int val2;

   if(lookahead=='+') {
      match('+');
      val2=term();
      val=val+val2;
      val=moreterms(val);
   } else if(lookahead=='-') {
      match('-');
      val2=term();
      val=val-val2;
      val=moreterms(val);
   }
   return val;
}

unsigned long expr()
{
   unsigned long val;

   val=term();
   val=moreterms(val);
   return val;
}

void label()
{
   int p;
   unsigned long val;

   p=lookupsym(lexbuf);
   if(!p)
      p=insertsym(lexbuf,ID);
   symtable[p].value=programcounter;
   match(ID);
   /* Optional colon */
   if(lookahead=='=' || lookahead==ASM_EQU) {
      lookahead=lexan();
      /* Not a label, but a constant */
      val=expr();
      symtable[p].value=val;
   } if(lookahead==':')
      match(':');
}

void emit1(unsigned char b)
{
   if(minpos>programcounter)
      minpos=programcounter;
   if(maxpos<programcounter)
      maxpos=programcounter;
   outfile[programcounter++]=b;
}

void emit2(unsigned char b,unsigned char c)
{
   if(minpos>programcounter)
      minpos=programcounter;
   if(maxpos<programcounter+1)
      maxpos=programcounter+1;
   outfile[programcounter++]=b;
   outfile[programcounter++]=c;
}

void emit3(unsigned char b,unsigned char c,unsigned char d)
{
   if(minpos>programcounter)
      minpos=programcounter;
   if(maxpos<programcounter+2)
      maxpos=programcounter+2;
   outfile[programcounter++]=b;
   outfile[programcounter++]=c;
   outfile[programcounter++]=d;
}

void emit4(unsigned char b,unsigned char c,unsigned char d,unsigned char e)
{
   if(minpos>programcounter)
      minpos=programcounter;
   if(maxpos<programcounter+3)
      maxpos=programcounter+3;
   outfile[programcounter++]=b;
   outfile[programcounter++]=c;
   outfile[programcounter++]=d;
   outfile[programcounter++]=e;
}

int determinetype(int addressmode,unsigned long val)
{
   if(type)
      return type;
   if(val<0x100)
      return 1;
   else if(val<0x10000)
      return 2;
   else
      return 3;
}

void instruction()
{
   char mne[4];
   int addressmode,i;
   unsigned long val,val4;
   unsigned char opcode;
   unsigned char val1,val2,val3;

   type=0;
   strcpy(mne,symtable[tokenval].lexptr);
   match(ASM_MNEMONIC);
   if(lookahead==ASM_A) {
      match(ASM_A);
      addressmode=10;             /* accumulator */
   } else if(lookahead=='\n' || lookahead==DONE)
      addressmode=0;              /* Implied */
   else if(lookahead=='#') {
      match('#');
      val=expr();
      addressmode=1;              /* Immediate */
      /* Check if it's 16-bit immediate */
      if((!strcmp(mne,"lda") || !strcmp(mne,"cmp") || !strcmp(mne,"ora")
         || !strcmp(mne,"and") || !strcmp(mne,"eor") || !strcmp(mne,"adc")
         || !strcmp(mne,"sbc") || !strcmp(mne,"bit")) && accusize==1)
         addressmode=12;
      else if((!strcmp(mne,"ldx") || !strcmp(mne,"cpx") || !strcmp(mne,"ldy")
         || !strcmp(mne,"cpy")) && indexsize==1)
         addressmode=12;
   } else if(lookahead=='(') {
      match('(');
      val=expr();
      type=determinetype(addressmode,val);
      if(lookahead==',') {
         match(',');
         if(lookahead==ASM_X) {
            match(ASM_X);
            match(')');
            if(type>=2)
               addressmode=22; /* Absolute indirect,x */
            else
               addressmode=7;  /* Direct indirect,x */
         } else if(lookahead==ASM_S) {
            match(ASM_S);
            match(')');
            match(',');
            match(ASM_Y);
            addressmode=21; /* Stack relative indirect,y */
         } else
            error("Illegal addressing mode.\n");
         goto doneaddr;
      } else {
         match(')');
         if(lookahead==',') {
            match(',');
            match(ASM_Y);
            addressmode=8; /* Direct indirect,y */
         } else {
            if(type>=2)
               addressmode=9; /* Absolute indirect */
            else
               addressmode=15; /* Direct indirect */
         }
         goto doneaddr;
      }
   } else if(lookahead=='[') {
      match('[');
      val=expr();
      type=determinetype(addressmode,val);
      match(']');
      if(lookahead==',') {
         match(',');
         match(ASM_Y);
         addressmode=17; /* Direct indirect long,y */
      } else if(type>=2)
         addressmode=23; /* Absolute indirect long */
      else
         addressmode=16; /* Direct indirect long */
   } else {
      val=expr();
      type=determinetype(addressmode,val);
      if(lookahead==',') {
         match(',');
         if(lookahead==ASM_S) {
            match(ASM_S);
            addressmode=20; /* Stack relative */
         } else if(lookahead==ASM_X) {
            match(ASM_X);
            if(type==1)
               addressmode=4; /* Direct page,x */
            else if(type==2)
               addressmode=5; /* Absolute,x */
            else
               addressmode=19; /* Absolute long,x */
         } else if(lookahead==ASM_Y) {
            match(ASM_Y);
            if(type>=2)
               addressmode=6; /* Absolute,y */
            else
               addressmode=14; /* Direct page,y */
         } else {
            val4=expr();
            val=(val & 0xFF) | ((val4 >> 8) & 0xFF);
            addressmode=24; /* Block move */
         }
      } else {
         if(!strcmp(mne,"bcc") || !strcmp(mne,"bcs") || !strcmp(mne,"beq")
            || !strcmp(mne,"bne") || !strcmp(mne,"bpl") || !strcmp(mne,"bmi")
            || !strcmp(mne,"bvc") || !strcmp(mne,"bvs") || !strcmp(mne,"bra")
            || !strcmp(mne,"bge") || !strcmp(mne,"blt"))
            addressmode=11; /* Relative */
         else if(!strcmp(mne,"brl") || !strcmp(mne,"per"))
            addressmode=13; /* Relative long */
         else if(type==1)
            addressmode=2;  /* Direct page */
         else if(type==2)
            addressmode=3;  /* Absolute */
         else
            addressmode=18; /* Absolute long */
      }
   }

doneaddr:
   i=0;
   while(mne6502[i].addr<255) {
      if(!strcmp(mne6502[i].mne,mne)) {
         if(mne6502[i].addr==addressmode) {
            opcode=mne6502[i].b;
            goto foundmne;
         } else if(mne6502[i].addr==10 && addressmode==0) {
            opcode=mne6502[i].b;
            goto foundmne;
         }
     }
     i++;
   }
   i=0;
   while(mne65816[i].addr<255 && opt_c==3) {
      if(!strcmp(mne65816[i].mne,mne)) {
         if(mne65816[i].addr==addressmode) {
            opcode=mne65816[i].b;
            goto foundmne;
         } else if(mne65816[i].addr==10 && addressmode==0) {
            opcode=mne65816[i].b;
            goto foundmne;
         }
     }
     i++;
   }
   i=0;
   while(mne6510[i].addr<255 && opt_c==2) {
      if(!strcmp(mne6510[i].mne,mne)) {
         if(mne6510[i].addr==addressmode) {
            opcode=mne6510[i].b;
            goto foundmne;
         } else if(mne6510[i].addr==10 && addressmode==0) {
            opcode=mne6510[i].b;
            goto foundmne;
         }
     }
     i++;
   }
   error("Illegal addressing mode.\n");

foundmne:
   val1=val & 0xFF;
   val2=(val >> 8) & 0xFF;
   val3=(val >> 16) & 0xFF;
   switch(addressmode) {
   case 0: case 10:
      emit1(opcode);
      break;
   case 1: case 2: case 4: case 7: case 8: case 14: case 15: case 16: case 20:
   case 21:
      emit2(opcode,val1);
      break;
   case 3: case 5: case 6: case 9: case 12: case 22:
      emit3(opcode,val1,val2);
      break;
   case 18: case 19:
      emit4(opcode,val1,val2,val3);
      break;
   case 11: /* pc relative */
      emit2(opcode,(val-programcounter-2) & 0xFF);
      break;
   case 13: /* pc relative long */
      val4=(val-programcounter-2) & 0xFFFF;
      emit3(opcode,val4 & 0xFF,(val4 >> 8) & 0xFF);
   }
   if(opcode==0xC2 && opt_c==3) { /* rep */
      if(val1 & 0x20)
         accusize=1;
      if(val1 & 0x10)
         indexsize=1;
   } else if(opcode==0xE2 && opt_c==3) { /* sep */
      if(val1 & 0x20)
         accusize=0;
      if(val1 & 0x10)
         indexsize=0;
   }
}

void parsebytetext()
{
   unsigned long val;
   int i;

   while(1) {
      if(lookahead==STR) {
         for(i=0;i<strlen(lexbuf);i++)
            emit1(lexbuf[i]);
         match(STR);
      } else {
         val=expr() % 0xFF;
         emit1(val);
      }
      if(lookahead!=',')
         break;
      match(',');
   }
}

void parseword()
{
   unsigned long val;

   while(1) {
      val=expr() % 0xFFFF;
      emit2(val & 0xFF,val >> 8);
      if(lookahead!=',')
         break;
      match(',');
   }
}

void parseprocessor()
{
   if(lookahead==NUM) {
      if(tokenval==6502)
         opt_c=1;
      else if(tokenval==6510)
         opt_c=2;
      else if(tokenval==65816)
         opt_c=3;
      else
         error("Invalid processor type, should be 6502, 6510 or 65816.\n");
      match(NUM);
   } else
      error("Number (6502, 6510 or 65816) expected.\n");
}

void assemble()
{
   if((outfile=(unsigned char *)malloc(65536))==NULL)
      error("Error allocating out buffer.\n");
   minpos=65536;
   maxpos=0;
   initsymtable();

   for(pass=1;pass<3;pass++) {
      lineno=1;
      infilepos=4;
      lookahead=lexan();
      while(lookahead!=DONE) {
         while(lookahead=='\n')
            lookahead=lexan();
         if(lookahead==DONE)
            break;
         /* *=x assign new value to program counter */
         if(lookahead=='*')
            modifypc();
         else if(lookahead==':') {
            match(':');
            label();
         } else if(lookahead==ID)
            label();
         else if(lookahead==ASM_MNEMONIC)
            instruction();
         else if(lookahead==ASM_PROCESSOR) {
            match(ASM_PROCESSOR);
            parseprocessor();
         } else if(lookahead=='.') {
            match('.');
            if(lookahead==ASM_TEXT) {
               match(ASM_TEXT);
               parsebytetext();
            } else if(lookahead==ASM_BYTE) {
               match(ASM_BYTE);
               parsebytetext();
            } else if(lookahead==ASM_WORD) {
               match(ASM_WORD);
               parseword();
            } else if(lookahead==ASM_PROCESSOR) {
               match(ASM_PROCESSOR);
               parseprocessor();
            } else
               error("Syntax error.\n");
         } else
            error("Syntax error.\n");
      }
   }
   printf("Assembly O.K.\n");
}

void saveoutputfiles()
{
   FILE *f;
   char b;

   f=fopen(outfilename,"wb");
   if(!f)
      return;
   b=minpos & 0xFF;
   fwrite(&b,1,1,f);
   b=(minpos >> 8) & 0xFF;
   fwrite(&b,1,1,f);
   fwrite(outfile+minpos,1,maxpos-minpos+1,f);
   fclose(f);
   unloadfile(infile);
   free(outfile);
}

int main(int argc,char **argv)
{
   header();
   parsecommandline(argc,argv);
   loadinputfiles();
   preprocess();
   assemble();
   saveoutputfiles();
   return 0;
}
