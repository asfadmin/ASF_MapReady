#include <stdio.h>
#ifdef SGI
#include <strings.h>
#else
#include <string.h>
#endif
#include <stdlib.h>
#include <ctype.h>
#include "defs.h"
#include "extern.h"
#include <sys/types.h>
#include <netinet/in.h>

static char sccsid_io_utils_c[] = "@(#)io_utils.c	1.11 96/10/04 14:42:12";

static int calc_length;
static int record_length;
static int global_get_position;
static int global_set_position;
extern int Record_length, Current_length, COM_entry;

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


int Check_Rec_Seq( int* val, unsigned char* tmp, SARL_ptr* tree, int mode) {
   int i=0;
    unsigned int *itmp;

    itmp = (unsigned int *)tmp;

   if (mode==1) {
      i = htonl( *itmp );
      tree->read_count++;
      if (tree->read_count != i) i=tree->read_count;
   }
   else if (!mode) {
      if (!calc_length) {
         tree->write_count++;
         if (tree->write_count != *val) {
	    *itmp = htonl( tree->write_count );
            i=tree->write_count;
         }
         else {
            i=*val;
	    *itmp = htonl( i );
         }
      }
   }
   else {
      printf("Error: Check_Rec_Seq  -  Neither Read or Write mode\n");
   }

   return (i);
}



/************************************************************************/
/* read nitems of record into "buf" */
int read_record( FILE* fp, unsigned char* buf, size_t size, size_t nitems)
{
   int nbyte;

   nbyte = fread(buf,sizeof(unsigned char),nitems,fp);

   if (feof(fp)) {
      printf("\n End of volume file reached\n");
      fflush(stdout);
      return(END_OF_FILE);
   }

   if (nbyte!=nitems) {
      printf("\n Error: Read requested %d items ______ returning %d",nitems,nbyte);
      return(READ_ERROR);
   } 

   return(nbyte);
}

/************************************************************************/
/* write nitems of buf  to file */
int write_record( FILE* fp, unsigned char * buf, size_t size, size_t nitems)
{
   int nbyte;
   
   if (calc_length) return(nitems);
   nbyte = fwrite(buf,sizeof(unsigned char),nitems,fp);


   if (nbyte!=nitems) {
      printf("\n Error: writing requested %d items ______ returning %d",nitems,nbyte);
      return(READ_ERROR);
   } 
   return(nbyte);
}

/************************************************************************/
void explicit_get_buf( int offset) {
    global_get_position = offset; 
}

void explicit_set_buf( int offset ) {
    global_set_position = offset;
}

int whatis_get_pos( void ) {
    return( global_get_position);
}

int whatis_set_pos( void ) {
    return( global_set_position);
}

/************************************************************************/
/* get integer out of ASCII field X chars wide */
int get_I4(unsigned char *cbuf, int field)
{
   char tmp[32];
   int x=0;

   strncpy(tmp, (char*) cbuf, field);
   tmp[field]='\0';
   sscanf(tmp,"%d",&x);
   global_get_position += field;
   return(x);
}

/************************************************************************/
/* get float out of ASCII field X chars wide */
float get_F4(unsigned char *cbuf, int field)
{
   char tmp[32];
   float x=0.0;

   strncpy(tmp, (char*) cbuf,field);
   tmp[field]='\0';
   sscanf(tmp,"%f",&x);
   global_get_position += field;
   return(x);
}


/************************************************************************/
/* get integer out of ASCII field X chars wide */
int Get_I4(unsigned char *cbuf, int field)
{
   char tmp[32];
   int x=0;

   Current_length += field;
   if (Current_length <= Record_length) {
      COM_entry++;
      strncpy(tmp, (char*) cbuf, field);
      tmp[field]='\0';
      sscanf(tmp,"%d",&x);
      global_get_position += field;
   }
   return(x);
}

/************************************************************************/
/* get float out of ASCII field X chars wide */
float Get_F4(unsigned char *cbuf, int field)
{
   char tmp[32];
   float x=0.0;

   Current_length += field;
   if (Current_length <= Record_length) {
      COM_entry++;
      strncpy(tmp, (char*) cbuf, field);
      tmp[field]='\0';
      sscanf(tmp,"%f",&x);
      global_get_position += field;
   }
   return(x);
}

/************************************************************************/
/* get char out of ASCII field X chars wide */
void strNcpy(char *dbuf, char *sbuf, int field)
{

   Current_length += field;
   if (Current_length <= Record_length) {
      COM_entry++;
      strncpy(dbuf, sbuf, field);
      dbuf[field]='\0';
      global_get_position += field;
   }
   else
      dbuf[0] = '\0';
}

/************************************************************************/
/* get unsigned char string  X chars wide from an integer */
void put_I4(int n, char* field, int ifield, unsigned char* buf)
{
   if (calc_length) 
      record_length+=ifield;
   else 
      sprintf( (char*) buf, field, n);
   global_set_position += ifield;

}

/************************************************************************/

void put_F4(float val, char * field, int ifield, unsigned char* buf)
{
   if (calc_length) 
      record_length+=ifield;
   else 
      sprintf((char*) buf, field, val);
   global_set_position += ifield;
}

/************************************************************************/

void put_byte(unsigned char* out, unsigned char in)
{
   if (calc_length) 
      record_length++;
   else 
      *out = in;
   global_get_position++;
}

/************************************************************************/

void put_chars( char* s1, char* s2, int ifield)
{
   int len;

   if (calc_length) {
      len = strlen(s2);
      if (len!=ifield) {
         printf("\n Warning: Trying to calculate record length\n\t input string length %d, stated length %d --- put_chars", len, ifield);
         record_length +=len;
      }
      else
         record_length+=ifield;
   }
   else 
      strncpy(s1,s2,ifield);
   global_set_position += ifield;

   /* Ensure that the proper number of characters were moved;
      blank-pad if not. */
   if ((int) strlen(s2) < (int) ifield)
     put_blanks(&s1[strlen(s2)], ifield - strlen(s2)); 
}

/************************************************************************/

void put_blanks( char* s1, int ifield) 
{
   int i;
   if (calc_length) 
      record_length+=ifield;
   else 
      for(i=0;i<ifield;i++) *(s1+i) = ' ';
   global_set_position += ifield;
}

/************************************************************************/

void printfd( char* buf, int value)
{
   if (COM_entry-- > 0) {
     printf(buf, value);
   }
}

/************************************************************************/

void printfl( char* buf, long value)
{
   if (COM_entry-- > 0) {
     printf(buf, value);
   }
}

/************************************************************************/

void printfs( char* buf, char* value)
{
   if (COM_entry-- > 0) {
     printf(buf, value);
   }
}

/************************************************************************/

void printff( char* buf, float value)
{
   if (COM_entry-- > 0) {
     printf(buf, value);
   }
}

/************************************************************************/

void printfe( char* buf, double value)
{
   if (COM_entry-- > 0) {
     printf(buf, value);
   }
}

/************************************************************************/

void printfdl( char* buf, int value1, long value2)
{
   if (COM_entry-- > 0) {
     printf(buf, value1, value2);
   }
}

/************************************************************************/

void printfdf( char* buf, int value1, float value2)
{
   if (COM_entry-- > 0) {
     printf(buf, value1, value2);
   }
}

/************************************************************************/

void zero_set( unsigned char* buf, int nitems)
{
    if (!calc_length) memset((char *)buf,0,nitems);
    global_set_position = 0;
}

/************************************************************************/

/* get file name out of non-null-terminated char field */
void get_file_name(unsigned char *cbuf,int clen,char *fname)
{
   char tmpbuf[100];
   int i;

   for (i = 0; i < clen; i ++)
      tmpbuf[i] = (char) cbuf[i];
   tmpbuf[clen] = (char) 0;
   sscanf(tmpbuf,"%s",fname);
}

void get_chars( char* s1, char* s2, int ifield) {
    strncpy(s1, s2, ifield); 
    global_set_position +=ifield;
}
 

#if 0
/************************************************************************/
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

   printf("unknown file type\n");
   exit(0);
}
#endif


