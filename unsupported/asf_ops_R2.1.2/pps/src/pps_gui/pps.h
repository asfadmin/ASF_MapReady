/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#ifndef _PPS_H_INCLUDED_
#define _PPS_H_INCLUDED_

#pragma ident	"@(#)pps.h	1.1  11/21/96"

typedef struct globalDataS
{
    char*    configFile;
    Boolean  showSQL;

} GlobalData, *GlobalDataPtr;

#endif /*_PPS_H_INCLUDED_*/
