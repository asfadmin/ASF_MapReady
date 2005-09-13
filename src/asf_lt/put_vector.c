/*******************************************************************************
FUNCTIONS:	put_vector		
PURPOSE:	Write label vector into Labeled Table I/O buffer

PROGRAM HISTORY:
  Version  Date   Author       	Request
  -------  ----	  ------    	-------
    1      12/86  K. Gacke      Original development
    2	    3/87  K. Gacke	Modified open_tab to allow directories to be 
				specified for the subfiles.  Replaced "strcat" 
				calls with "strcpy" to make the code more 
				efficient.  Replaced "sscanf" calls with "atol"
				and "atof" to make the code more efficient.
    3       5/87  K. Gacke      Modified "put_vector" to search the link list 
				for each attribute defined.
    4	    6/87  K. Gacke	Allow the LT to be opened for append.  Modified
				"close_tab", "get_vector", "open_tab", and 
				"put_vector".
    5	    7/87  K. Gacke	changed the calls malloc and free to xalloc and
				xree for the sel.  These routines are the C run
				time routines renamed and put into the newlas.a
				library to avoid linking the malloc and free in
				the fortran libraries.
    6	    8/87  K. Gacke	added the DATE and TIME data types.  Modified
				the routines get_vector, get_record, put_record,
				and put_vector.  Created the routines
				get_lt_date, get_lt_time, put_lt_date, and
				put_lt_time.
    7       9/87  K. Gacke	removed mod for "version 5" after installing
				new OS (UTX 2.0)
    8      12/87  B. Ailts	Change include directory structure
    9      05/88  B. Ailts	Standardized err messages
				Split routines into separate files
				Added worgen.h
   10      02/90  B. Ailts      Checked for allocation errors

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
    PUT_VECTOR writes the label vector to the Labeled Table file.
*******************************************************************************/
#include "asf.h"


#include <ctype.h>

#include "worgen.h"
#include "ltable.h"

char DATA_TYPE[NDTYPE][7] = {"C","I1","I2","I4","R4","R8","DATE","TIME"};
void put_i4_field(struct TAB_DEF *tab,int val,char sep);

void put_vector(
    struct TAB_DEF *tab,	/*structure defining LT file to be processed. */
    char label[],		/*label name of the vector		      */
    int dtype,			/*data type of the label vector		      */
    char desc[],		/*description of the label vector	      */
    int nrow,			/*number of rows			      */
    int ncol,			/*number of columns			      */
    char sep)			/*separator (FIELD_SEP1,FIELD_SEP2,RECORD_SEP)*/
{
struct VECTOR *next_vec;
struct VECTOR *temp;
struct VECTOR *vec;
int size;
char msg[ERRLEN];

if ((tab->label_flag == TRUE) && (tab->access != 2))
   {
   sprintf(msg,
      "Tried to write to the label vector after data was written to LT \"%s\".",
      tab->hname);
   tab_error(tab,TAB_LT_PUTVEC,FATAL,msg,"put_vector");
   return;
   }
if (tab->label_flag == FALSE)
   {
   if (sep == RECORD_SEP)
      {
      ++tab->currec;
      tab->label_flag = TRUE;
      }
   ++tab->ncol;
   put_string_field(tab,label,FIELD_SEP1,TRUE);
   put_string_field(tab,DATA_TYPE[dtype],FIELD_SEP1,TRUE);
   put_string_field(tab,desc,FIELD_SEP1,TRUE);
   put_i4_field(tab,nrow,FIELD_SEP1);
   put_i4_field(tab,ncol,sep);

   next_vec = (struct VECTOR *) MALLOC(sizeof(struct VECTOR));
   next_vec->label = MALLOC(strlen(label) + 1);
   strcpy(next_vec->label,label);
   next_vec->dtype = dtype;
   next_vec->desc = MALLOC(strlen(desc) + 1);
   
   strcpy(next_vec->desc,desc);
   next_vec->sep = FIELD_SEP1;
   size = nrow * ncol;
   switch (dtype)			/* allocate memory to hold values for */
     {					/*   the record field                 */
     case I1:
        next_vec->ptr.i1 = MALLOC(sizeof(char) * size);
        break;
     case I2:
        next_vec->ptr.i2 = (short *) MALLOC(sizeof(short) * size);
        break;
     case I4:
        next_vec->ptr.i4 = (int *) MALLOC(sizeof(int) * size);
        break;
     case R4:
        next_vec->ptr.r4 = (float *) MALLOC(sizeof(float) * size);
        break;
     case R8:
        next_vec->ptr.r8=(double *) MALLOC(sizeof(double) * size);
        break;
     case C:
     case LTDATE:
     case LTTIME:
        next_vec->ptr.c = MALLOC(MAX_FIELD_LEN);
        break;
     default:
        tab_error(tab,TAB_LT_DTYPE,FATAL,"","put_vector");
	return;
        break;
     }
   next_vec->lt_size.nrow = nrow;
   next_vec->lt_size.ncol = ncol;
   next_vec->next = NULL;
   if (tab->vector == NULL)		/* assign the current vector to the  */
      tab->vector = next_vec;		/*   end of the linked list          */
   else
      {
      for (vec=tab->vector; vec->next!=NULL; vec=vec->next)
	 ;
      vec->next = next_vec;
      }
   }
else
   {
   temp = get_field_ptr(tab,label);
   if (temp == NULL)
      {
      sprintf(msg,"Label name \"%s\" does not exist in the LT \"%s\".",
              label,tab->hname);
      tab_error(tab,TAB_LT_PUTVEC,FATAL,msg,"put_vector");
      return;
      }
   if (temp->dtype != dtype)
      {
      sprintf(msg,
              "Data type for label \"%s\" does not correspond with LT \"%s\".",
              label,tab->hname);
      tab_error(tab,TAB_LT_PUTVEC,FATAL,msg,"put_vector");
      return;
      }
   if ((temp->lt_size.nrow != nrow) || (temp->lt_size.ncol != ncol))
      {
      sprintf(msg,"%s\"%s\"%s\"%s\".","Number of rows or columns for label ",
              label," does not correspond with LT ",tab->hname);
      tab_error(tab,TAB_LT_PUTVEC,FATAL,msg,"put_vector");
      return;
      }
   temp->flag = TRUE;
   if (sep == RECORD_SEP)
      {
      for (temp=tab->vector; temp!=NULL; temp=temp->next)
         {
         if (temp->flag != TRUE)
            {
            sprintf(msg,"%s\"%s\"%s\"%s\"%s","Label name ",temp->label,
                    " exists in LT ",tab->hname,
                    ", but is not known to the program.");
            tab_error(tab,TAB_LT_PUTVEC,FATAL,msg,"put_vector");
            return;
            }
         }
      }
   }
return;
}
