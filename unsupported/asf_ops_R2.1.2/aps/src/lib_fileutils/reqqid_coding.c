#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       reqqid_coding.c
 
Description:    contains routines to encode/decode the darid, path, and row
                into an REQQ id, which will also be the REQA id
 
External Functions Defined:
encode()
 
Notes:
        This file written with a 4-character tab setting. 
 
==============================================================================*/
#pragma ident   "@(#)reqqid_coding.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_fileutils/SCCS/s.reqqid_coding.c"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include <grs_utilities.h>


/*==============================================================================
Function:       reqqid_is_rsp()

Description:    Checks input 8-char REQQ ID, returns TRUE if a RSP request.  

Creator:        Lawrence Stevens

Creation Date:  Fri Oct  3 10:29:55 PDT 1997

==============================================================================*/
#include <ctype.h>       /* for isdigit()     */
#include <dapps_defs.h>  /* for TRUE and FALSE    */

int  reqqid_is_rsp( char  *REQQ_id )   /* returns TRUE if RSP REQQid    */
{
    /*
    --  In an RSP request, the characters are:
    --  "Srrrrrdd"
    --  Where S = 'S'
    --  rrrrr is the rev number, 0-filled, 
    --  dd is the dtkid, 0-filled.  
    --
    --  If the input REQQ_id has the first character != 'S', 
    --  or if any of the remaining characters are not 0-9, 
    --  then the REQQ_id is a GRS request, not a RSP request, 
    --  and FALSE is returned.  
    */

    /* 
    -- order the column tests for 
    -- the quickest probable decision.  
    */

    if( REQQ_id[0] != 'S' )              /* 1st char is not S  */  
        return FALSE ;

    if( ! isdigit( REQQ_id[1] )  )       /* 2nd char is not 0-9   */
        return FALSE ;

    if( ! isdigit( REQQ_id[2] )  )       /* 3rd char is not 0-9   */
        return FALSE ;

    if( ! isdigit( REQQ_id[7] )  )       /* Last char is not 0-9   */
        return FALSE ;

    if( ! isdigit( REQQ_id[3] )  )
        return FALSE ;

    if( ! isdigit( REQQ_id[4] )  )
        return FALSE ;

    if( ! isdigit( REQQ_id[5] )  )
        return FALSE ;

    if( ! isdigit( REQQ_id[6] )  )
        return FALSE ;

    /* the REQQ_id is an RSP request.  */
    return TRUE ;

}


/*==============================================================================
Function:      encode()

Description:   Given a base ten number, will encode it into a single digit in
                 base 94.  This is the number of different characters that
                 are possible on the keyboard, and correspond to ASCII values
                 33 through 126 continuously.  Going to a higher base number
                 with ASCII characters is probably possible, but that would
                 require an ASCII value above 128, and the symbols in that
                 region vary between different languages.  Keeping the values
                 under 127 removes this problem, so there should be no
                 decoding problems due to language encountered.

Parameters:
    Input Parameters:
    digit               long           the digit in base 10

    Output Parameters:
    digitchar           char *         the digit in base 94

Returns:       void

Creator:       Brian J Griglak

Creation Date: Mon Jul 21 18:29:12 PDT 1997
==============================================================================*/

static void encode(long digit, char *digitchar)
{
    *digitchar = '!' + digit;

    return;
}


static long decode(char digitchar)
{
    long digit;

    digit = digitchar - '!';

    return  digit;
}



/*==============================================================================
Function:      reqqid_encode()

Description:   Given the darid, path, and row number, this creates a unique
                 representation for them in 8 characters.  The first four
                 are the darid in base 36 (this is why darid is a long, it
                 allows for darid > MAXINT).  The next two are the path, and
                 the last two are the row, both in base 26.  Base 26 is large
                 enough, since PP in base 26 is 675, and the largest path
                 is 659, and largest row is 449.

Parameters:
    Input Parameters:
    darid               long           the darid in base 10
    path, row           int            path and row in base 10

    Output Parameters:
    reqqid              char *         the unique, eight character reqqid
                                         for this darid, path, row set

Returns:       void

Creator:       Brian J Griglak

Creation Date: Mon Jul 21 18:33:43 PDT 1997
==============================================================================*/

void reqqid_encode(
    long darid,
    int path,
    int row,
    int phase,
    int image,
    char reqqid[]  )
{
    ldiv_t digit;
    long remainder;
    long path_row;
    char digitchar;

    encode(phase, &digitchar);
    reqqid[0] = digitchar;

    digit = ldiv(darid, 8836);
    remainder = digit.rem;
    encode(digit.quot, &digitchar);
    reqqid[1] = digitchar;

    digit = ldiv(remainder, 94);
    remainder = digit.rem;
    encode(digit.quot, &digitchar);
    reqqid[2] = digitchar;

    encode(digit.rem, &digitchar);
    reqqid[3] = digitchar;

    path_row = path * 1000 + row;

    digit = ldiv(path_row, 8836);
    remainder = digit.rem;
    encode(digit.quot, &digitchar);
    reqqid[4] = digitchar;

    digit = ldiv(remainder, 94);
    remainder = digit.rem;
    encode(digit.quot, &digitchar);
    reqqid[5] = digitchar;

    encode(digit.rem, &digitchar);
    reqqid[6] = digitchar;

    encode(image, &digitchar);
    reqqid[7] = digitchar;

    return;
}

/*==============================================================================
Function:      reqqid_decode()

Description:   Given the reqqid, this breaks it up and retrieves the darid,
                 path, and row numbers.  I was able to take advantage of the
                 strtol command to clean up this routine.  Unfortunately there
                 is no inverse for strtol, which is basically what was done
                 in encode().

Parameters:
    Input Parameters:
    reqqid              char *         the unique, eight character reqqid
                                         for this darid, path, row set
    Output Parameters:
    darid               long *         the darid in base 10
    path, row           int *          path and row in base 10


Returns:       void

Creator:       Brian J Griglak

Creation Date: Mon Jul 21 18:37:36 PDT 1997
==============================================================================*/

void reqqid_decode(
    char reqqid[],
    long *darid,
    int *path,
    int *row,
    int *phase,
    int *image  )
{
    long path_row;

    *phase = decode(reqqid[0]);

    *darid = 8836 * decode(reqqid[1]);
    *darid += 94 * decode(reqqid[2]);
    *darid += decode(reqqid[3]);
 
    path_row = 8836 * decode(reqqid[4]);
    path_row += 94 * decode(reqqid[5]);
    path_row += decode(reqqid[6]);

    *path = path_row / 1000;
    *row = path_row % 1000;

    *image = decode(reqqid[7]);

    return;
}


