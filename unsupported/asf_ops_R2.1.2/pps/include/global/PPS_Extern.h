/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Description:
            function prototypes for PPS public functions
==============================================================================*/

#ifndef _PPS_EXTERN_H
#define _PPS_EXTERN_H

#pragma ident "@(#)PPS_Extern.h	1.1  11/21/96"

int PPS_extract_param_value(AGGREGATE obj, void* rec,
                            char* keyword, short length);

#endif /* _PPS_EXTERN_H */
