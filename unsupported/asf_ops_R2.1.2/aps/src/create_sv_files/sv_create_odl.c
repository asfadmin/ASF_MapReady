#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		sv_create_odl.c

Description:	Create ODL state vector file

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)sv_create_odl.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/create_sv_files/SCCS/s.sv_create_odl.c"

#include "create_sv_file.h"

/*
** Local Functions.
*/
 
void create_ODL_common_hdr(ODL_COMMON_HEADER *, AGGREGATE);
void create_ODL_metadata(ODL_SV_METADATA * ,AGGREGATE );
void create_ODL_data(IMS_SV_STRUCT *, AGGREGATE);

int sv_create_odl(COMMANDS *commands, IMS_SV_STRUCT *pnt_retbuf)
{
    int status;
    int sv_record_count;  
    
    IMS_SV_STRUCT * temp_pnt_retbuf;
    IMS_SV_STRUCT * pnt_retbuf2;
    ODL_COMMON_HEADER *ODL_common_header; 
    ODL_SV_METADATA *ODL_sv_metadata; 
    AGGREGATE root, parent, child;


   /*
    ** create ODL labels
    */
 
    temp_pnt_retbuf = pnt_retbuf;
    sv_record_count = 0;

    /* count number of records retreived */
    while( temp_pnt_retbuf  !=  (IMS_SV_STRUCT *) NULL  )
    {
        sv_record_count++;
        temp_pnt_retbuf = temp_pnt_retbuf->next;
    }
 
    if (sv_record_count == 0)
       return(FALSE);

    ODL_common_header = 
					(ODL_COMMON_HEADER *) malloc( sizeof( ODL_COMMON_HEADER ) );
    ODL_sv_metadata = (ODL_SV_METADATA *) malloc(sizeof( ODL_SV_METADATA ));

    root = NewAggregate(NULL,KA_OBJECT,"root",NULL);
    parent = NewAggregate(root,KA_OBJECT,SV_OBJECT_NAME,NULL);
 
 
    /* fill in common header struct and create common header aggregate */
 
    strcpy (ODL_common_header->file_creation_time,commands->odl_creation_time);
    strcpy (ODL_common_header->file_type,SV_MSG_TYPE_NAME);
    strcpy (ODL_common_header->file_dest,commands->dest);
    strcpy (ODL_common_header->file_source,SV_SRC_NAME);
    sprintf (ODL_common_header->number_of_records,"%d",sv_record_count);
 
    create_ODL_common_hdr(ODL_common_header,parent);
 
   /* fill in metadata structure */
 
    strcpy (ODL_sv_metadata->platform,commands->platform);
 
    if (strcmp(commands->precision, "P") == 0)
        strcpy (ODL_sv_metadata->metadata_type,"PREDICTED");
    else
        strcpy (ODL_sv_metadata->metadata_type,"RESTITUTED");
 
    strcpy (ODL_sv_metadata->coordinate,"TRUE_EQUATORIAL");
 
    /* fill in and create all sv metadata and sv data records */
 
    while(pnt_retbuf != (IMS_SV_STRUCT *) NULL)
    {
        child = NewAggregate(parent,KA_OBJECT,SV_RECORD_NAME,NULL);
        create_ODL_metadata(ODL_sv_metadata,child);
        create_ODL_data(pnt_retbuf,child);
        pnt_retbuf = pnt_retbuf->next;
	}
 
	WriteLabel (commands->SV_file_ptr, root);
	RemoveAggregate(root) ;
 
 
    /*
    ** free the results.
    */
    while( pnt_retbuf  !=  (IMS_SV_STRUCT *) NULL  )
    {
        pnt_retbuf2 = pnt_retbuf->next;
        free( pnt_retbuf );
        pnt_retbuf = pnt_retbuf2;
    }
 
  return(TRUE);
 

} /* sv_create_odl */

/*==============================================================================
Function:	create_ODL_common_hdr
Description:	Creates the common header aggregate for the SV
Parameters:     ODL_Common_Header *record - C structure containing the values
                                            of the fields in the common header
		AGGREGATE parent - the parent aggregate, which should be the SV
		                   aggregate
Returns:	
Creator:        Tieh Ku	
Creation Date:	11/14/95
Notes:		
==============================================================================*/
void create_ODL_common_hdr(ODL_COMMON_HEADER *record, AGGREGATE parent)
{
  AGGREGATE  common_hdr ;       /* common header aggregate */
  PARAMETER  curr_param ;       /* the current parameter   */
  VALUE_DATA curr_value ;       /* the current value       */

  /* create the COMMON_HDR aggregate, and connect it to parent */
  
  common_hdr = NewAggregate(parent, KA_OBJECT, SV_COMMON_HDR, NULL) ;

  /* create the TIME parameter, assign it to common_hdr aggregate, and attach
  -- the file_creation_time value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, SV_TIME) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (record->file_creation_time, sizeof(record->file_creation_time)-1,1) ;
  NewValue(curr_param, &curr_value) ;
  
  /* create the MSG_TYPE parameter, assign it to common_hdr aggregate, and 
  -- attach the file_type value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, SV_MSG_TYPE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (record->file_type, sizeof(record->file_type)-1, 2) ;
  NewValue(curr_param, &curr_value) ;


  /* create the DESTINATION parameter, assign it to common_hdr aggregate, and 
  -- attach the file_source value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, SV_DEST) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol(record->file_dest, sizeof(record->file_dest)-1, 2) ;
  NewValue(curr_param, &curr_value) ;
 

  /* create the SOURCE parameter, assign it to common_hdr aggregate, and attach
  -- the file_source value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, SV_SRC) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol(record->file_source, sizeof(record->file_source)-1, 2) ;
  NewValue(curr_param, &curr_value) ;
  
  /* create the NUMBER_OF_RECORDS parameter, assign it to common_hdr aggregate, and 
  -- attach the #_recs value to it
  */
  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, SV_NUMRECS) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertInteger(record->number_of_records, sizeof(record->number_of_records)-1) ;
  NewValue(curr_param, &curr_value) ;
 
 
} /* create_ODL_common_hdr */



 

/*==============================================================================
Function:	create_ODL_metadata
Description:    create SV metadata record
Returns:	
==============================================================================*/
void create_ODL_metadata(ODL_SV_METADATA *record, AGGREGATE child)
{
 
  AGGREGATE metadata ;
  PARAMETER  curr_param ;
  VALUE_DATA curr_value ;
 
  /* create the METADATA aggregate, and connect it to child note */

  metadata = NewAggregate(child,KA_OBJECT,SV_METADATA_NAME,NULL) ;

  /*create platform parameter,assign it to metadata aggregate and attach value to it*/
      curr_param = NewParameter(metadata,KP_ATTRIBUTE,SV_PLATFORM) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->platform, sizeof(record->platform)-1, 2) ;
      NewValue(curr_param, &curr_value) ;
    
  /*create type parameter,assign it to metadata aggregate and attach value to it*/
      curr_param = NewParameter(metadata,KP_ATTRIBUTE,SV_METADATA_TYPE) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->metadata_type, sizeof(record->metadata_type)-1, 2) ;
      NewValue(curr_param, &curr_value) ;

  /*create coordinate parameter,assign it to metadata aggregate and attach value to it*/
      curr_param = NewParameter(metadata,KP_ATTRIBUTE,SV_COORDINATE) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->coordinate, sizeof(record->coordinate)-1, 2) ;
      NewValue(curr_param, &curr_value) ;

} /* create_ODL_metadata */



 

/*==============================================================================
Function:	create_ODL_data
Description:	Creates SV data object 

Returns:	None
Creator: 	Tieh Ku	
Creation Date:	11/16/1995
Notes:		
==============================================================================*/
void create_ODL_data(IMS_SV_STRUCT *pnt_retbuf,AGGREGATE child)
{
 
  AGGREGATE state_vector_data ;
  PARAMETER  curr_param ;
  VALUE_DATA curr_value ;
 
 
  /* create the SV data aggregate, and connect it to child note */

  state_vector_data = NewAggregate(child,KA_OBJECT,SV_DATA_NAME,NULL) ;

  /*create revolution parameter and attach its value*/

      curr_param = NewParameter(state_vector_data,KP_ATTRIBUTE,SV_REVOLUTION) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (pnt_retbuf->rev, sizeof(pnt_retbuf->rev)-1, 1) ;	
      NewValue(curr_param, &curr_value) ;
   

  /*create time parameter and attach its value*/

      curr_param = NewParameter(state_vector_data,KP_ATTRIBUTE,SV_TIME) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (pnt_retbuf->date, sizeof(pnt_retbuf->date)-1, 1) ;
      NewValue(curr_param, &curr_value) ;

  /*create x_position parameter and attach its value*/

      curr_param = NewParameter(state_vector_data,KP_ATTRIBUTE,SV_X_POS) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (pnt_retbuf->x_pos, sizeof(pnt_retbuf->x_pos)-1, 1) ;
      NewValue(curr_param, &curr_value) ;


  /*create y_position parameter and attach its value*/

      curr_param = NewParameter(state_vector_data,KP_ATTRIBUTE,SV_Y_POS) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (pnt_retbuf->y_pos, sizeof(pnt_retbuf->y_pos)-1, 1) ;
      NewValue(curr_param, &curr_value) ;

 
  /*create z_position parameter and attach its value*/

      curr_param = NewParameter(state_vector_data,KP_ATTRIBUTE,SV_Z_POS) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (pnt_retbuf->z_pos, sizeof(pnt_retbuf->z_pos)-1, 1) ;
      NewValue(curr_param, &curr_value) ;


  /*create x_velocity parameter and attach its value*/

      curr_param = NewParameter(state_vector_data,KP_ATTRIBUTE,SV_X_VEL) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (pnt_retbuf->x_vel, sizeof(pnt_retbuf->x_vel)-1, 1) ;
      NewValue(curr_param, &curr_value) ;

 
  /*create y_velocity parameter and attach its value*/

      curr_param = NewParameter(state_vector_data,KP_ATTRIBUTE,SV_Y_VEL) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (pnt_retbuf->y_vel, sizeof(pnt_retbuf->y_vel)-1, 1) ;
      NewValue(curr_param, &curr_value) ;


  /*create z_velocity parameter and attach its value*/

      curr_param = NewParameter(state_vector_data,KP_ATTRIBUTE,SV_Z_VEL) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (pnt_retbuf->z_vel, sizeof(pnt_retbuf->z_vel)-1, 1) ;
      NewValue(curr_param, &curr_value) ;

} /* create_ODL_data */


