#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	CSAconversions.h
Description:header for CSA conversion routines, which are used to decode/
			translate CSA File string values.
Creator:	Miguel Siu
Notes:		
==============================================================================*/
#pragma ident	"@(#)CSAconversions.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.CSAconversions.h"


#ifndef _CSAconversions_
#define _CSAconversions_

#include <fa_defs.h>

extern CSA_VALUEDEF sch_req_file[];
extern CSA_FILENAME CSA_files[] ;
extern EQUIV_TABLE CSA_dtk_filetype[] ;

int CSAc_time2asftime(
	void 		*unused,  /* equivalence table not used by this routine */
	char 		*source_string, 
	char		*result_string);

int CSAc_append_station(
    EQUIV_TABLE *facility_to_suffix,
    char        *facility,
    char        *file_type );

int CSAc_get_activity(
    EQUIV_TABLE *CSA_activity_type,
    char        *unused_station,
    char        *activity_type );

int CSAc_default_quicklook(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	char 		*quicklook) ;

int CSAc_default_sensor(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	char 		*result);

int CSAc_default_darid(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	int 		*result);

#endif /* _CSAconversions_ */
