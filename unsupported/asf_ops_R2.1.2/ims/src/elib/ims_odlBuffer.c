static char *sccs = "@(#)ims_odlBuffer.c	5.1  16 Mar 1996";
/* **************************************************************
**
** File:        ims_odlBuffer.c
**
** Function:    Message facility.
**
** Author:      David Pass
**
** Date:        10/17/95
**
** Modified:
**
**************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_timeConv.h>
#include <ims_keyword.h>

#include <ims_odlBuffer.h>

static  int get_next_token( IMS_MSG_STRUCT *, short, char *,
    char *, short *, short *, long * );


/* **************************************************************
**
**  subr ims_parseOdlBuffer () - parse odl buffer to a linked list of
**      structures which contain the name and value.  this replaces
**      the ReadLabel_buf call, which did not work.  this is not
**      a general solution, although it could be expanded to one.
**
**************************************************************** */
IMS_KEYWORD_LIST * ims_parseOdlBuffer (
    IMS_MSG_STRUCT *msgDesc,
    char *odlBuffer,
    char *targetAggregate)
{
    short  flag;
    pnt_ims_keyword_list_t pnt_odl; /* current buffer  */
    pnt_ims_keyword_list_t pnt_odl_temp;
    pnt_ims_keyword_list_t pnt_odl_1st; /* first buffer */
    short  n_odl; /* current pointer within the current buffer  */
    short  n_odl_tot; /* total no. of old sets  */
    /*
    **  I am using a buffer to copy the odlBuffer so that memory does
    **  not get segmented due to DCE problems.  all the strings are
    **  in this buffer, with the endstrings.
    */
    static char  odl_buf[MAX_ODL_BUF];
    short  n_char; /* char at in buffer: start of token  */
    short  nc_last; /* start of last token  */
    short  nc_end;  /* end of current token  */
    short  nc_start;
    short  add_next;
    int  status;
    short  num_errors;
    long   n_line;  /* line of current token  */
    long   n_line_last; /* line of last token  */


    pnt_odl_1st = (pnt_ims_keyword_list_t) malloc(
        sizeof( IMS_KEYWORD_LIST ));
    pnt_odl = pnt_odl_1st;
    pnt_odl->next = NULL;
    (void) strcpy( odl_buf, odlBuffer );
    n_char = 0;
    n_odl = -1;
    n_odl_tot = 0;
    n_line_last = 0;
    /*
    **  this scheme reads tokens, which are delineated by a blank,
    **  end-of-line, or equals mark.  if an equals mark, put the
    **  tokens before and after in the list.  make sure the lists have
    **  an end mark.  (note:  this is why I copied the buffer, as eol
    **  marks have to be inserted.)
    */
    flag = IMS_TRUE;
    add_next = IMS_FALSE;
    num_errors = 0;
    while( flag ){
        status = get_next_token( msgDesc, n_char, odlBuffer,
            odl_buf, &nc_start, &nc_end, &n_line );
        if(  status  <  IMS_OK )
        {
            num_errors++;
        }
        if(  nc_start  ==  MAX_ODL_BUF )
        { /* finished buffer */
            flag = IMS_FALSE;
        }
        else
        {
            /*
            ** if the token is an =, then add pair
            */
            if(  odlBuffer[nc_start]  ==  '=' )
            { /*  have equal: need to save pair */
                add_next = IMS_TRUE;
            }
            else  if(  add_next )
            {
                /*
                **  add this and last token to pair
                */
                add_next = IMS_FALSE;
                n_odl++;
                n_odl_tot++;
                if(  n_odl  ==  MAX_ODL  )
                { /*  need a new buffer  */
                    pnt_odl_temp = (pnt_ims_keyword_list_t) malloc(
                        sizeof( IMS_KEYWORD_LIST ));
                    pnt_odl->next = pnt_odl_temp;
                    pnt_odl = pnt_odl_temp;
                    pnt_odl->next = (pnt_ims_keyword_list_t) NULL;
                    n_odl = 0;
                }
                pnt_odl->ary[n_odl].keyword = &odl_buf[nc_last];
                pnt_odl->ary[n_odl].value = &odl_buf[nc_start];
            }
            else
            {
                /*
                ** this is now the last buffer
                */
                nc_last = nc_start;
                n_line_last = n_line;
            }
        }
        n_char = nc_end;
    }
    /*
    ** the last pair set to null to indicate end
    */
    n_odl++;
    n_odl_tot++;
    if(  n_odl  ==  MAX_ODL  )
    { /*  need a new buffer  */
        pnt_odl_temp = (pnt_ims_keyword_list_t) malloc(
            sizeof( IMS_KEYWORD_LIST ));
        pnt_odl->next = pnt_odl_temp;
        pnt_odl = pnt_odl_temp;
        pnt_odl->next = (pnt_ims_keyword_list_t) NULL;
        n_odl = 0;
    }
    pnt_odl->ary[n_odl].keyword = NULL;
    pnt_odl->ary[n_odl].value = NULL;
    if(  num_errors  >  0  )
    {
        return( NULL );
    }
    return( pnt_odl_1st );
}   /*  ims_parseOdlBuffer */



/* **************************************************************
**
**  subr get_next_token gets the next token.  the tokens are stoped
**      by blanks, tabs, eol marks, or an equal sign.  the equal
**      sign is a token in itself.  quotes are allowed:  the first
**      char must be a quote mark.  only double quotes allowed.
**  nc_at       position in buffer to start
**  buf         odl buffer: original, do not change
**  chg_buf     odl buffer: to be changed
**  nc_start    start of next token
**  nc_end      end of next token
**
**************************************************************** */
static  int get_next_token( IMS_MSG_STRUCT * msgDesc, short nc_at,
    char * buf, char * chg_buf, short * nc_start, short * nc_end,
    long * n_lines_out )
{
    long  i;
    short  flag;
    static char  blank = ' ';
    static char  tab = 9;
    static char equals = '=';
    static char eol = '\n';
    short  start;
    static char  quote = '"';
    static long n_lines = 1;

    if(  nc_at  ==  0  )  n_lines = 1;
    if(  buf[nc_at]  ==  '\0' ){
        /*
        **  buffer ended: no token
        */
        *nc_start = MAX_ODL_BUF;
        *nc_end = MAX_ODL_BUF;
        *n_lines_out = n_lines;
        return( IMS_OK );
    }

    /*
    ** first skip to first non-skip char (blank,tab).
    */
    flag = IMS_TRUE;
    for( i=nc_at ; flag ; i++ )
    {
        if( buf[i]  ==  blank || buf[i]  ==  tab  ) ;
        else  if( buf[i]  ==  eol ) n_lines++;
        else  flag = IMS_FALSE; /* non-skip character */
    }
    i--;
    /*
    **  now go to end of tab.  check first if =.
    */
    if(  buf[i]  ==  equals ){ /* this is next token */
        *nc_start = i;
        *nc_end = i+1;
        *n_lines_out = n_lines;
        return( IMS_OK );
    }
    /*
    **  check if quote mark.
    */
    if(  buf[i]  ==  quote ){ /* go to next non-double quote mark */
        flag = IMS_TRUE;
        start = i;
        for( i = i+1 ; flag ; i++ )
        {
            if(  buf[i]  ==  quote )
            {
                if(  i-1  ==  start )
                {   /* quote quote: this is nothing in string */
                    flag = IMS_FALSE;
                    *nc_start = start+1;
                    *nc_end = start+2;
                    *n_lines_out = n_lines;
                    chg_buf[start+1] = '\0';
                }
                else  if(  buf[i-1]  ==  quote  ||  buf[i+1]  ==
                    quote ) ;
                else{ /* this is end of quote */
                    *nc_start = start+1;
                    *nc_end = i+1;
                    *n_lines_out = n_lines;
                    chg_buf[i] = '\0';
                    flag = IMS_FALSE;
                }
            }
            else  if( buf[i]  ==  eol )
            { /* no end quote on line */
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Quotes not ended on line %d", n_lines );
                *nc_start = start+1;
                *nc_end = i+1;
                *n_lines_out = n_lines;
                n_lines++;
                chg_buf[i] = '\0';
                flag = IMS_FALSE;
            }
            else  if( buf[i]  ==  '\0' )
            { /* no end quote on line */
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Quotes not ended on line %d", n_lines );
                *nc_start = start+1;
                *nc_end = i;
                *n_lines_out = n_lines;
                n_lines++;
                chg_buf[i] = '\0';
                flag = IMS_FALSE;
            }
        }
    }
    else{
        /*
        **  in this case, go to next skip char: blank,tab,eol
        */
        flag = IMS_TRUE;
        start = i;
        for( i=i+1 ; flag ; i++ )
        {
            if(  buf[i]  ==  blank  ||  buf[i]  ==  tab ){
                *nc_start = start;
                *nc_end = i+1;
                *n_lines_out = n_lines;
                chg_buf[i] = '\0';
                flag = IMS_FALSE;
            }
            else if(  buf[i]  ==  eol ){
                *nc_start = start;
                *nc_end = i+1;
                *n_lines_out = n_lines;
                chg_buf[i] = '\0';
                flag = IMS_FALSE;
                n_lines++;
            }
            else  if( buf[i]  ==  '\0' ){
                *nc_start = start;
                *nc_end = i;
                *n_lines_out = n_lines;
                chg_buf[i] = '\0';
                flag = IMS_FALSE;
                n_lines++;
            }
        }
    }
    return( IMS_OK );
}   /*  get_next_token  */
