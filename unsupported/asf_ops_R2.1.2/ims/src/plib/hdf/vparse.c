/***************************************************************************
*
*
*                         NCSA HDF version 3.3r1
*                            September 20, 1993
*
* NCSA HDF Version 3.3 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
*
* We ask, but do not require, that the following message be included in all
* derived works:
*
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign, in collaboration with the
* Information Technology Institute of Singapore.
*
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
*
*****************************************************************************/

#include "vg.h"

#define ISCOMMA(c) ( (c==',') ? 1:0 )

/* ------------------------------------------------------------------ */

/*
** Given a string (attrs) , the routine parses it into token strings,
** and returns a ptr (attrv) to an array of ptrs where the tokens 
** are stored.  The number of tokens are returned in attrc.
**
** Currently used only by routines that manipulate field names.
** As such each field string is truncated to a max length of
** FIELDNAMELENMAX (as defined in hdf.h). For most cases, this
** truncation doesn't happen because FIELDNAMELENMAX is a big number.
**
** RETURN FAIL if error.
** RETURN SUCCEED if ok.
**
** Current implementation: all strings inputs converted to uppercase.    
** tokens must be separated by COMMAs.
**
** Tokens are stored in static area sym , and pointers are returned
** to calling routine. Hence, tokens must be used before next call 
** to scanattrs.
**
*/
#if defined(macintosh) | defined(THINK_C)
PRIVATE char** 	symptr = NULL; /* array of ptrs to tokens  ?*/
PRIVATE char**  sym = NULL;    /* array of tokens ? */
#else  /* !macintosh */
PRIVATE char* 	symptr[50]; /* array of ptrs to tokens  ?*/
PRIVATE char    sym[50][FIELDNAMELENMAX+1]; /* array of tokens ? */
#endif /* !macintosh */
PRIVATE	intn 	nsym;   /* token index ? */

#ifdef PROTOTYPE
int32 scanattrs (char *attrs, int32 *attrc, char ***attrv)
#else
int32 scanattrs (attrs,attrc,attrv)

	char	*attrs;		/* field string (input) */
	int32	*attrc;		/* # of fields (output) */
	char	***attrv;	/* array of char ptrs to fields (output) */
#endif

{
  register char   *s, *s0, *ss;
  register intn   i, slen, len;
  char * FUNC = "scanattrs";
  char * saved_string = (char *) HDstrdup(attrs);

#if defined(macintosh) | defined(THINK_C)
  /* Lets allocate space for ptrs to tokens and tokens */
  if (symptr == NULL)
    {
      symptr = (char **)HDgetspace(50 * sizeof(char *));
      if (symptr == NULL)
          HRETURN_ERROR(DFE_NOSPACE, FAIL);
    }
  if (sym == NULL)
    {
      sym = (char **)HDgetspace(50 * sizeof(char *));
      if (sym == NULL)
          HRETURN_ERROR(DFE_NOSPACE, FAIL);

      for (i = 0; i < 50; i++)
        {
      	  sym[i] = (char *)HDgetspace(sizeof(char) * (FIELDNAMELENMAX + 1));
      	  if (sym[i] == NULL)
              HRETURN_ERROR(DFE_NOSPACE, FAIL);
      	}
     }
#endif /* macintosh */
 
  s = saved_string;
  slen = HDstrlen(s);
  nsym = 0;
  
  s0 = s;
  while(*s) {
      if (*s >= 'a' && *s <= 'z') *s=(char)toupper(*s);
      if ( ISCOMMA(*s) ) {

          /* make sure we've got a legitimate length */
          len = (intn)(s - s0);
          if (len <= 0) return(FAIL);
          
          /* save that token */
          ss = symptr[nsym] = sym[nsym]; 
          nsym++;
          
          /* shove the string into our static buffer.  YUCK! */
          if(len>FIELDNAMELENMAX) len=FIELDNAMELENMAX;
          HIstrncpy(ss, s0, len+1);

          /* skip over the comma */
          s++;

          /* skip over white space before the next field name */
          while(*s && *s == ' ') s++;
          
          /* keep track of the first character of the next token */
          s0 = s;

      } else {

          /* move along --- nothing to see here */
          s++;
      }
  }  
  
  /* save the last token */
  len = (intn)(s - s0);
  if (len <= 0) return(FAIL);
  ss = symptr[nsym] = sym[nsym]; 
  nsym++;
  
  if(len>FIELDNAMELENMAX) len=FIELDNAMELENMAX;
  HIstrncpy(ss, s0, len+1);
  
  symptr[nsym] = NULL;
  *attrc = nsym;
  *attrv = (char**) symptr;
  
  HDfreespace(saved_string);

  return(SUCCEED); /* ok */
  
} /* scanattrs */

/* ------------------------------------------------------------------ */
