static char *sccs = "@(#)ims_ioUtils.c	5.1  03/17/96";
#include    <stdio.h>
#include    <string.h>
#include    <stdlib.h>
#include    <ctype.h>
#include    <sys/types.h>

#include    "../lincl/defs.h"
#include    "../lincl/extern.h"
#include    <ims_dbms.h>
#include    <ims_const.h>
#include    <ims_msg.h>
#include    <ims_qi.h>


static int calc_length;
static int record_length;

FILE* Open_LDR( char* name, int mode) {
   FILE *fp;

   if  ( mode == READ_ONLY )
      fp = fopen(name, "r");
   else if ( mode == WRITE_ONLY )
      fp = fopen(name, "w");
   else
      fp = ( FILE *) NULL;
   return(fp);
}
void recalc_length(void)
{
   calc_length=TRUE;
}

void reset_rec_length(void)
{
   record_length=0;
   calc_length=FALSE;
}

int get_rec_length(void)
{
   return(record_length);
}

int get_recalc_status(void)
{
   return(calc_length);
}


int Check_Rec_Seq( int* val, unsigned char* tmp, SARL_ptr* tree,
   int mode) {
   int i=0;

   if (mode==1) {
      i = cvt2int( tmp );
      tree->read_count++;
      if (tree->read_count != i) i=tree->read_count;
   }
   else if (!mode) {
      if (!calc_length) {
         tree->write_count++;
         if (tree->write_count != *val) {
            cvt2char(&(tree->write_count), tmp);
            i=tree->write_count;
         }
         else {
            cvt2char( val, tmp );
            i=*val;
         }
      }
   }
   else {
      (void) printf(
        "Error: Check_Rec_Seq  -  Neither Read or Write mode\n");
   }

   return (i);
}



/* ************************************************************* */
/* read nitems of record into "buf" */
int read_record( FILE* fp, unsigned char* buf, size_t size,
size_t nitems)
{
   int j, nbyte;
   unsigned char *tptr, rval;

   nbyte = fread(buf,sizeof(unsigned char),nitems,fp);

   if (feof(fp)) {
      /* (void) printf("\n End of volume file reached\n");  */
      (void) fflush(stdout);
      return(END_OF_FILE);
   }

   if (nbyte!=nitems) {
      /* (void) printf(
    "\n Error: Read requested %d items ______ returning %d",nitems,
            nbyte);
        */
      return(READ_ERROR);
   }
#if 0
#ifdef SWAP_READ
   for (j=0,tptr= (unsigned char *) buf;j<nitems;j+=4,tptr+=4) {
       rval = *tptr;
       *tptr = *(tptr+3);
       *(tptr+3) = rval;
       rval = *(tptr+1);
       *(tptr+1) = *(tptr+2);
       *(tptr+2) = rval;
   }
#endif
#endif

   return(nbyte);
}

/* ***************************************************************** */
/* write nitems of buf  to file */
int write_record( FILE* fp, unsigned char * buf, size_t size,
    size_t nitems)
{
   int j, nbyte;
   unsigned char *tptr, rval;

   if (calc_length) return(nitems);
#if 0
#ifdef SWAP_WRITE
   for (j=0,tptr = buf;j<nitems;j+=4,tptr+=4) {
       rval = *tptr;
       *tptr = *(tptr+3);
       *(tptr+3) = rval;
       rval = *(tptr+1);
       *(tptr+1)=*(tptr+2);
       *(tptr+2)=rval;
   }
#endif
#endif
   nbyte = fwrite(buf,sizeof(unsigned char),nitems,fp);


   if (nbyte!=nitems) {
      /* (void) printf(
        "\n Error: writing requested %d items ______ returning %d",
            nitems,nbyte);
        */
      return(READ_ERROR);
   }
   return(nbyte);
}



/* ************************************************************* */
/* convert 4 bytes to an integer */
int cvt2int(unsigned char *cint)
{
   unsigned char *cnum;
   int num;
   int i;

   cnum = (unsigned char *) &num;
   for (i=0;i<4;i++) {
#if DEC
      cnum[3-i] = cint[i];
   }
#else
      cnum[i] = cint[i];
   }
#endif

   return(num);
}

/* ************************************************************* */
/* get integer out of ASCII field X chars wide */
int get_I4(unsigned char *cbuf, int field)
{
   char tmp[32];
   int x=0;

   (void) strncpy(tmp, (char*) cbuf, field);
   tmp[field]='\0';
   (void) sscanf(tmp,"%d",&x);
   return(x);
}

/* ************************************************************** */
/* get float out of ASCII field X chars wide */
float get_F4(unsigned char *cbuf, int field)
{
   char tmp[32];
   float x=0.0;

   (void) strncpy(tmp, (char*) cbuf,field);
   tmp[field]='\0';
   (void) sscanf(tmp,"%f",&x);
   return(x);
}

/* ************************************************************* */
/* convert 4 bytes to an integer */
void cvt2char(int *val, unsigned char* cint)
{
   unsigned char *cnum;
   int i;

   if (calc_length) {
      record_length+=4;
   }
   else {
      cnum = (unsigned char *) val;
      for (i=0;i<4;i++) {
#if DEC
         cint[3-i] = cnum[i];
#else
         cint[i] = cnum[i];
#endif
      }
   }
}

/* ************************************************************* */
/* get unsigned char string  X chars wide from an integer */
void put_I4(int n, char* field, int ifield, unsigned char* buf)
{
   if (calc_length)
      record_length+=ifield;
   else
      (void) sprintf( (char*) buf, field, n);

}

/* ************************************************************** */

void put_F4(float val, char * field, int ifield, unsigned char* buf)
{
   if (calc_length)
      record_length+=ifield;
   else
      (void) sprintf((char*) buf, field, val);
}

/* ************************************************************* */

void put_byte(unsigned char* out, unsigned char in)
{
   if (calc_length)
      record_length++;
   else
      *out = in;
}

/* ************************************************************* */

void put_chars( char* s1, char* s2, int ifield)
{
   int len;

   if (calc_length) {
      len = strlen(s2);
      if (len!=ifield) {
         (void) printf(
"\n Warning: Trying to calculate record length\n\t input string \
length %d, stated length %d --- put_chars", len, ifield);
         record_length +=len;
      }
      else
         record_length+=ifield;
   }
   else
      (void) strncpy(s1,s2,ifield);
}

/* ********************************************************* */

void put_blanks( char* s1, int ifield)
{
   int i;
   if (calc_length)
      record_length+=ifield;
   else
      for(i=0;i<ifield;i++) *(s1+i) = ' ';
}

/* *********************************************************** */

void zero_set( unsigned char* buf, int nitems)
{
    if (!calc_length) (void) memset((char *)buf,0,nitems);
}

/* ********************************************************* */

/* get file name out of non-null-terminated char field */
int get_file_name(unsigned char *cbuf,int clen,char *fname)
{
   char tmpbuf[100];
   int i;

   for (i = 0; i < clen; i ++)
      tmpbuf[i] = (char) cbuf[i];
   tmpbuf[clen] = (char) 0;
   (void) sscanf(tmpbuf,"%s",fname);
   return IMS_OK;
}

#if 0
/* ************************************************************* */
/* get (return) file type (IMOP, SARL, or SART) */
int get_file_type(unsigned char *cbuf,int clen)
{
   char tmpbuf[100];
   int i;

   for (i = 0; i < clen; i ++)
      tmpbuf[i] = (char) cbuf[i];
   tmpbuf[clen] = (char) 0;

   for (i = 0; i < KNOWN_FILES; i ++)
      if (strcmp(cftype[i],tmpbuf) == 0)
         return(i);

   (void)printf("unknown file type\n");
   exit(0);
}
#endif


