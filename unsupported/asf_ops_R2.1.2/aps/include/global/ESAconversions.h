#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	ESAconversions.h
Description:header for ESA conversion routines, which are used to decode/
			translate ESA File string values.
Creator:	Miguel Siu
Notes:		
==============================================================================*/
#pragma ident	"@(#)ESAconversions.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/global/SCCS/s.ESAconversions.h"

#ifndef _ESAconversions_
#define _ESAconversions_

#include "fa_defs.h"

int ESAc_aps_report(
	EQUIV_TABLE	*table,
	char 		*keyword_value, 
	int 		*result);

int ESAc_yymmdd_pad_hh_mm_ss2asftime(
	void 		*unused,  /* equivalence table not used by this routine */
	char 		*source_string, 
	char		*result_string);

int ESAc_time_duration2asftime(
	void 		*unused,  /* equivalence table not used by this routine */
	char 		*source_string, 
	char		*result_string);

int ESAc_yyyymmddhhmmssccc2asftime(
	void 		*unused,  /* equivalence table not used by this routine */
	char 		*source_string, 
	char		*result_string);

int ESAc_dd_mmm_yy_hh_mm_ss_ccc2asftime(
	void 		*unused,  /* equivalence table not used by this routine */
	char 		*source_string, 
	char		*result_string);

int ESAc_default_sensor(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	char 		*result);

int ESAc_default_shaq_sensor(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	char 		*result);

int ESAc_default_darid(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	int 		*result);

int ESAc_default_transid(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	char 		*result);

int ESAc_default_actid(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	char		*result);

int ESAc_default_shaq_actid(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	char		*result);

int ESAc_default_shaqp_stat(
    void        *unused_pointer,
    char        *unused_pointr2,
    char        *status);

int ESAc_default_shaqp_type(
    void        *unused_pointer,
    char        *unused_pointr2,
    char        *status);

int ESAc_default_subrecords(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	int	 		*result);

int ESAc_default_subrec_size(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	int	 		*result);

int ESAc_set_READ_TO_EOF(
	void		*unused_pointer,
	char 		*unused_pointr2, 
	int	 		*result);

int ESAc_get_shaqp_sat(
    void        *unused_pointer,
    char        *unused_pointr2,
    char        *sat);

int ESAc_get_shaqp_creation_date(
    void        *unused_pointer,
    char        *unused_pointr2,
    char        *creation_date);

int ESAc_shaqp_time_duration2strttime(
    void        *unused_pointer,
    char        *time_duration_string,
    char        *strttime);

int ESAc_shaqp_time_duration2stoptime(
    void        *unused_pointer,
    char        *time_duration_string,
    char        *stoptime);

extern EQUIV_TABLE	ESA_dtk_status[];
extern EQUIV_TABLE	ESA_facility_id[];
extern VALUE_DEFS	ESA_valuedefs_shaqp[];
extern FA_FILEDEF 	ESA_filedef_shaqp;
extern VALUE_DEFS	ESA_valuedefs_shaq[];
extern FA_FILEDEF 	ESA_filedef_shaq;
extern VALUE_DEFS	ESA_valuedefs_gap[];
extern FA_FILEDEF 	ESA_filedef_gap;
extern FA_FILENAME	ESA_files[] ;

#endif /* _ESAconversions_ */
