/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	PMF.c

Description:	
	Contains the functions for building the Product Metadata File
information (PMF)

External Functions:
	create_ODL_common_hdr
	create_ODL_catalog_metadata
	create_ODL_detailed_metadata
        create_ODL_dsm_catalog_metadata
        create_PMF
        create_dsm_PMF
	PMFcurrtime
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  Oct. '96 - RH
    Updated to handle ASF_Data_Seq_Msg_Record:
      Added create_PMF() and create_ODL_dsm_catalog_metadata()
2.  June '97 - R. Hoffman
    In every ODLConvertSymbol() and ODLConvertInteger(), change
       sizeof(...)-1 to strlen(...) to get rid of extraneous CRs.
==============================================================================*/

static char SccsFile[] = "PMF.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)PMF.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "ODLcommonhdr.h"
#include "PMF.h"
#include "WALPS.h"


/*==============================================================================
Function:	create_ODL_common_hdr
Description:	Creates the common header aggregate for the PMF
Parameters:     ODL_Common_Header *record - C structure containing the values
                                            of the fields in the common header
		AGGREGATE parent - the parent aggregate, which should be the PMF
		                   aggregate
Returns:	
Creator:	Philip Yurchuk (phil@orca.jpl.nasa.gov)
Creation Date:	6/25/95
Notes:		
==============================================================================*/
void create_ODL_common_hdr(ODL_Common_Header *record, AGGREGATE parent)
{
  AGGREGATE  common_hdr ;       /* common header aggregate */
  PARAMETER  curr_param ;       /* the current parameter   */
  VALUE_DATA curr_value ;       /* the current value       */
  char tempdate[TIME_STRING_LEN+1] ;

  /* create the COMMON_HDR aggregate, and connect it to parent */
  
  common_hdr = NewAggregate(parent, KA_OBJECT, PMFKEYWD_COMMON_HDR, NULL) ;

  /* create the TIME parameter, assign it to common_hdr aggregate, and attach
  -- the file_creation_time value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_TIME) ;
  curr_param->value_kind = KV_SCALAR ;
  strcpy(tempdate, record->file_creation_time) ;
  curr_value = ODLConvertDateTime (tempdate, strlen(tempdate)) ;
  NewValue(curr_param, &curr_value) ;
  
  /* create the SOURCE parameter, assign it to common_hdr aggregate, and attach
  -- the file_source value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_SRC) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol(record->file_source, strlen(record->file_source), 2) ;
  NewValue(curr_param, &curr_value) ;
  
  /* create the DESTINATION parameter, assign it to common_hdr aggregate, and 
  -- attach the file_source value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_DEST) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol(record->file_dest, strlen(record->file_dest), 2) ;
  NewValue(curr_param, &curr_value) ;
 
  /* create the NUMBER_OF_RECORDS parameter, assign it to common_hdr aggregate, and 
  -- attach the #_recs value to it
  */
  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_NUMRECS) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertInteger(record->number_of_records, strlen(record->number_of_records)) ;
  NewValue(curr_param, &curr_value) ;
 
  /* create the MSG_TYPE parameter, assign it to common_hdr aggregate, and 
  -- attach the file_type value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_MSG_TYPE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (record->file_type, strlen(record->file_type), 2) ;
  NewValue(curr_param, &curr_value) ;
 
} /* create_ODL_common_hdr */



 
/*==============================================================================
Function:	create_ODL_catalog_metadata
Description:	Creates the catalog metadata aggregate for the PMF
Parameters:     ODL_Catalog_Metadata *record - C structure containing the values
                                   of the fields in the catalog metadata object
		AGGREGATE parent - the parent aggregate, which should be the PMF
		                   aggregate
Returns:	
Creator:        Philip Yurchuk   (phil@orca.jpl.nasa.ogv)
Creation Date:	7/5/95
Notes:		
==============================================================================*/
void create_ODL_catalog_metadata(ODL_Catalog_Metadata *record, AGGREGATE parent){
 
  AGGREGATE catalog_metadata ;
  PARAMETER  curr_param ;
  VALUE_DATA curr_value ;
  char tempdate[TIME_STRING_LEN+1] ;
 
  /* create the CATALOG_METADATA aggregate, and connect it to parent */

  catalog_metadata = NewAggregate(parent, KA_OBJECT, PMFKEYWD_CATMETA, NULL) ;

  if (record->file_name[0])
    {
      curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_FILENAME) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->file_name, strlen(record->file_name), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->format[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_FORMAT) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->format, strlen(record->format), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->file_source[0])
    {
      curr_param = NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_SRC) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->file_source, strlen(record->file_source), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->file_arrival_time[0])
    {
      curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_ARRIVE_TIME) ;
      curr_param->value_kind = KV_SCALAR ;
      strcpy(tempdate, record->file_arrival_time) ;
      curr_value = ODLConvertDateTime (tempdate, sizeof(tempdate)-1) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->file_dest[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_DEST) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->file_dest, strlen(record->file_dest), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->FA_file_type[0])
    {
      curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_FA_FTYPE) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->FA_file_type, strlen(record->FA_file_type), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->gen_file_type[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_GEN_FTYPE) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->gen_file_type, strlen(record->gen_file_type), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->satellite[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_PLATFORM) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->satellite, strlen(record->satellite), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->file_creation_time[0])
    {
      curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_FILE_CREAT_TIME) ;
      curr_param->value_kind = KV_SCALAR ;
      strcpy(tempdate, record->file_creation_time) ;
      curr_value = ODLConvertDateTime (tempdate, sizeof(tempdate)-1) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->valid_start_time[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_VALID_START) ;
      curr_param->value_kind = KV_SCALAR ;
      strcpy(tempdate, record->valid_start_time) ;
      curr_value = ODLConvertDateTime (tempdate, sizeof(tempdate)-1) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->valid_end_time[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_VALID_END) ;
      curr_param->value_kind = KV_SCALAR ;
      strcpy(tempdate, record->valid_end_time) ;
      curr_value = ODLConvertDateTime (tempdate, sizeof(tempdate)-1) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->start_rev[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_START_REV) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertInteger (record->start_rev, strlen(record->start_rev)) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->end_rev[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_END_REV) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertInteger (record->end_rev, strlen(record->end_rev)) ;  
      NewValue(curr_param, &curr_value) ;
    }

} /* create_ODL_catalog_metadata */



 
/*==============================================================================
Function:	create_ODL_dsm_catalog_metadata
Description:	Creates the catalog metadata aggregate for the PMF
Parameters:     ODL_Catalog_Metadata *record - C structure containing the values
                                   of the fields in the catalog metadata object
		AGGREGATE parent - the parent aggregate, which should be the PMF
		                   aggregate
Returns:	
Creator:        Rodney Hoffman
Creation Date:	October 1996
Notes:		Adapted from create_ODL_catalog_metadata()
==============================================================================*/
void create_ODL_dsm_catalog_metadata
    (ASF_Data_Seq_Msg_Record *dsm, AGGREGATE parent)
{
 
  AGGREGATE catalog_metadata ;
  PARAMETER  curr_param ;
  VALUE_DATA curr_value ;
  char tempdate[TIME_STRING_LEN+1] ;
  char numstr[80];
 
  /* create the CATALOG_METADATA aggregate, and connect it to parent */

  catalog_metadata = NewAggregate(parent, KA_OBJECT, PMFKEYWD_CATMETA, NULL) ;

  /* system_activity_id */
  curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_SYS_ACT_ID) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->system_activity_id, 
                      strlen(dsm->system_activity_id), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* system_activity_type */
  curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_SYS_ACT_TYPE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (dsm->system_activity_type, 
                        strlen(dsm->system_activity_type), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* status */
  curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_STATUS) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (dsm->status, strlen(dsm->status), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* platform */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_PLATFORM) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = 
         ODLConvertSymbol (dsm->platform, strlen(dsm->platform), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* sensor */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_SENSOR) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (dsm->sensor, strlen(dsm->sensor), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* rev */
  curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_REVOLUTION) ;
  curr_param->value_kind = KV_SCALAR ;
  sprintf (numstr, "%d", dsm->revolution); 
  curr_value = ODLConvertInteger (numstr, strlen(numstr)) ;
  NewValue(curr_param, &curr_value) ;

  /* sequence */
  curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_SEQUENCE) ;
  curr_param->value_kind = KV_SCALAR ;
  sprintf (numstr, "%d", dsm->sequence); 
  curr_value = ODLConvertInteger (numstr, strlen(numstr)) ;
  NewValue(curr_param, &curr_value) ;

  /* media_id_alias */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_MEDIA_ID_ALIAS) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->media_id_alias, strlen(dsm->media_id_alias), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* media_id */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_MEDIA_ID) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->media_id, strlen(dsm->media_id), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* media_id_type_name */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_MEDIA_ID_TYP) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (dsm->media_id_type_name, 
                      strlen(dsm->media_id_type_name), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* media_dog_tag */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_MEDIA_DOG_TAG) ;
  curr_param->value_kind = KV_SCALAR ;
  sprintf (numstr, "%d", dsm->media_dog_tag);
  curr_value = ODLConvertInteger (numstr, strlen(numstr)) ;
  NewValue(curr_param, &curr_value) ;

  /* generation */
  curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_GENERATION) ;
  curr_param->value_kind = KV_SCALAR ;
  sprintf (numstr, "%d", dsm->generation); 
  curr_value = ODLConvertInteger (numstr, strlen(numstr)) ;
  NewValue(curr_param, &curr_value) ;

  /* recorder_id */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_RECORDER_ID) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->recorder_id, strlen(dsm->recorder_id), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* recorder_type */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_RECORDER_TYPE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->recorder_type, strlen(dsm->recorder_type), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* recorder_media_type */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_RECORDER_MTYPE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->recorder_media_type, 
                      strlen(dsm->recorder_media_type), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* data_direction */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_DATA_DIRECTION) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->data_direction, strlen(dsm->data_direction), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* start_address */
  curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_START_ADDRESS) ;
  curr_param->value_kind = KV_SCALAR ;
  sprintf (numstr, "%d", dsm->start_address); 
  curr_value = ODLConvertInteger (numstr, strlen(numstr)) ;
  NewValue(curr_param, &curr_value) ;

  /* stop_address */
  curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_STOP_ADDRESS) ;
  curr_param->value_kind = KV_SCALAR ;
  sprintf (numstr, "%d", dsm->stop_address); 
  curr_value = ODLConvertInteger (numstr, strlen(numstr)) ;
  NewValue(curr_param, &curr_value) ;

  /* start_time */
  if (dsm->start_time.year > 0)  /* year = 0 for TPR which has no start_time */
  {
     curr_param =
	    NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_START_TIME) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempdate,"%04d-%03dT%02d:%02d:%02d.%03d",
       dsm->start_time.year,
       dsm->start_time.doy,
       dsm->start_time.hours,
       dsm->start_time.minutes,
       dsm->start_time.seconds,
       dsm->start_time.nanoseconds/1000000);
     curr_value = 
            ODLConvertDateTime (tempdate, sizeof(tempdate)-1) ;
     NewValue(curr_param, &curr_value) ;
  }

  /* end_time */
  if (dsm->end_time.year > 0)  /* year = 0 for TPR which has no end_time */
  {
     curr_param =
	    NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_END_TIME) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempdate,"%04d-%03dT%02d:%02d:%02d.%03d",
       dsm->end_time.year,
       dsm->end_time.doy,
       dsm->end_time.hours,
       dsm->end_time.minutes,
       dsm->end_time.seconds,
       dsm->end_time.nanoseconds/1000000);
     curr_value = 
            ODLConvertDateTime (tempdate, sizeof(tempdate)-1) ;
     NewValue(curr_param, &curr_value) ;
  }

  /* channelization */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_CHANNELIZATION) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->channelization, strlen(dsm->channelization), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* bit_rate */
  curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_BIT_RATE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol 
                     (dsm->bit_rate, strlen(dsm->bit_rate), 2) ;
  NewValue(curr_param, &curr_value) ;

  /* dsm from MSH has tdrs_number_of_ids = 0 (and it shouldn't appear in PMF), 
     dsm from TPR has tdrs_number_of_ids > 0 */
  if (dsm->tdrs_number_of_ids > 0)
  {
     curr_param =
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_TDRS_NUM_IDS) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf (numstr, "%d", dsm->tdrs_number_of_ids); 
     curr_value = ODLConvertInteger (numstr, strlen(numstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

} /* create_ODL_dsm_catalog_metadata */




/*==============================================================================
Function:	create_ODL_detailed_metadata
Description:	Creates DETAILED_METADATA object part of PMF	
Parameters:
	record - contains detailed metadata info to put in PMF ODL object
	parent - ODL tree to add detailed metadata object

Returns:	None
Creator:	Philip Yurchuk
Creation Date:	Thu Jun 29 15:27:37 PDT 1995
Notes:		
==============================================================================*/
void create_ODL_detailed_metadata(ODL_Detailed_Metadata *record, AGGREGATE parent)
{
 
  AGGREGATE detailed_metadata ;
  PARAMETER  curr_param ;
  VALUE_DATA curr_value ;
 
  detailed_metadata =
     NewAggregate(parent, KA_OBJECT, PMFKEYWD_DETAILEDMETA, NULL) ;
  curr_param = 
     NewParameter(detailed_metadata, KP_ATTRIBUTE, PMFKEYWD_FILESIZE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertInteger (record->file_size, sizeof(record->file_size)-1) ;
  NewValue(curr_param, &curr_value) ;
 
} /* create_ODL_detailed_metadata */




/*==============================================================================
Function:	PMFcurrtime	
Description:	Gets current time to be used in the PMF
Parameters:	None
Returns:	time string for current time	
Creator:	Phil Yurchuk
Creation Date:	Thu Jun 29 15:27:57 PDT 1995
Notes:		
==============================================================================*/
char *PMFcurrtime()
{
  char *s ;
  time_t now ;
  
  s = (char *)util_do_malloc(sizeof(char)*(TIME_STRING_LEN+1)) ;

  now = time(NULL) ;
  strftime(s, TIME_STRING_LEN+1, "%Y-%jT%X.000",localtime(&now)) ;

  return (s) ;

} /* PMFcurrtime */




/*==============================================================================
Function:	PMFfiletime
Description:	Get last modification time of a file (Part of PMF)
Parameters:
	fname - name of file to get mod time for
Returns:	Time string for mod time of fname	
Creator:	Phil Yurchuk
Creation Date:	Thu Jun 29 15:27:57 PDT 1995
Notes:		
==============================================================================*/
char *PMFfiletime(char *fname)
{
  char *s ;
  struct stat buf ;

  stat(fname, &buf) ;

  s = (char *)util_do_malloc(sizeof(char)*(TIME_STRING_LEN+1)) ;

  strftime(s, TIME_STRING_LEN+1, "%Y-%jT%X.000",localtime(&buf.st_mtime)) ;
  /*
  strftime(s, TIME_STRING_LEN+1, "%Y-%jT%X.000",localtime(&buf.st_mtim.tv_sec)) ;
  */

  return (s) ;

} /* PMFfiletime */

  
 

/*==============================================================================
Function:	create_PMF	
Description:	Create PMF object from metadata record supplied	
Parameters:
	catrecord - metadata record containing info to be put in PMFcatalog_meta
Returns:	Root of PMF ODL	
Creator:	Phil Yurchuk
Creation Date:	Thu Jun 29 15:28:21 PDT 1995
Notes:		
==============================================================================*/
AGGREGATE create_PMF(ODL_Catalog_Metadata *catrecord)
{
  
  AGGREGATE root, parent ;
  ODL_Common_Header *commonrecord ;
  ODL_Common_Header commonhdrrec ;

  commonrecord  = &commonhdrrec ;

  strcpy(commonrecord->file_creation_time, PMFcurrtime()) ;
  strcpy(commonrecord->file_source, PMF_SRC) ;
  strcpy(commonrecord->number_of_records, NUMRECS_IN_PMF) ;
  strcpy(commonrecord->file_dest, PMF_DEST) ;
  strcpy(commonrecord->file_type, FAIF_PMF_NAME) ;

  root   = NewAggregate(NULL, KA_OBJECT, "root", NULL) ;
  parent = NewAggregate(root, KA_OBJECT, FAIF_PMF_NAME, NULL) ;

  create_ODL_common_hdr(commonrecord, parent) ;
  create_ODL_catalog_metadata(catrecord, parent) ;

  return(root) ;

} /* create_PMF */

  
 

/*==============================================================================
Function:	create_dsm_PMF	
Description:	Create PMF object from dsm metadata record supplied	
Parameters:
	catrecord - dsm record containing info to be put in PMFcatalog_meta
Returns:	Root of PMF ODL	
Creator:	Rodney Hoffman
Creation Date:	October 1996
Notes:		Adapted from create_PMF()
==============================================================================*/
AGGREGATE create_dsm_PMF(ASF_Data_Seq_Msg_Record *dsm)
{
  
  AGGREGATE root, parent ;
  ODL_Common_Header *commonrecord ;
  ODL_Common_Header commonhdrrec ;

  commonrecord  = &commonhdrrec ;

  strcpy(commonrecord->file_creation_time, PMFcurrtime()) ;
  strcpy(commonrecord->file_source, PMF_SRC) ;
  strcpy(commonrecord->number_of_records, NUMRECS_IN_PMF) ;
  strcpy(commonrecord->file_dest, "IMS") ;
  strcpy(commonrecord->file_type, dsm->msg_type) ;

  root   = NewAggregate(NULL, KA_OBJECT, "root", NULL) ;
  parent = NewAggregate(root, KA_OBJECT, dsm->msg_type, NULL) ;

  create_ODL_common_hdr(commonrecord, parent) ;
  create_ODL_dsm_catalog_metadata(dsm, parent) ;

  return(root) ;

} /* create_dsm_PMF */


/* End of File */
