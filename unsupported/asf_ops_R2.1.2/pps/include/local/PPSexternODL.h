/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:       PPSexternODL.h
Description:
		PPS extern definitions which involve ODL
 
Creator:        Nadia Adhami (nadia.adhami@jpl.nasa.gov)
Notes:
==============================================================================*/
 
#ifndef _PPSEXTERNODL_
#define _PPSEXTERNODL_

#pragma ident "@(#)PPSexternODL.h	1.1  11/21/96"

/* lib pps functions */

extern void 	convert_ODLdate_to_timerec(struct ODLDate *, Time_Record *) ;
extern void 	create_CP_Framejob_buf(IMS_L1PReq_Record *, char *, int) ;
extern void 	create_CP_Scanjob_buf(IMS_ScanReq_Record *, char *, int) ;
extern void	create_order_status_buf(IMS_Order_Status *, char *, int) ;
extern void 	create_ODL_common_hdr(ODL_Common_Header *, AGGREGATE) ;
extern void 	create_ODL_order_status_body(IMS_Order_Status *, AGGREGATE) ;
extern void	create_ODL_order_status(IMS_Order_Status *, AGGREGATE *) ;
extern void 	create_ODL_CP_Framejob_body(IMS_L1PReq_Record *, AGGREGATE) ;
extern void 	create_ODL_CP_Framejob(IMS_L1PReq_Record *, AGGREGATE *) ;
extern void 	create_ODL_CP_Scanjob_body(IMS_ScanReq_Record *, AGGREGATE) ;
extern void 	create_ODL_CP_Scanjob(IMS_ScanReq_Record *, AGGREGATE *) ;
extern void 	create_ODL_GHA_body(GHA_Correction *, AGGREGATE) ;
extern void 	create_ODL_SVData_body(State_Vector *, AGGREGATE) ;
extern void 	create_ODL_SVmeta_body(State_Vector *, AGGREGATE) ;
extern void 	create_ODL_TCE_body(Time_Correlation *, AGGREGATE) ;

extern int 	extract_common_header(Common_Header_Record *, AGGREGATE) ;
extern int	extract_param_value(AGGREGATE, void *, char *, short) ;
extern int	extract_CP_JobStatus_Record(CP_JobStatus_Record *, AGGREGATE) ;
extern int 	extract_CP_JobReq_Record(CP_JobReq_Record *, AGGREGATE);
extern int 	extract_IMS_CancelReq_Record(IMS_CancelReq_Record *, AGGREGATE) ;
extern int 	extract_IMS_DubReq_Record(IMS_DubReq_Record *, AGGREGATE) ;
extern int 	extract_IMS_L1PReq_Record(IMS_L1PReq_Record *, AGGREGATE) ;
extern int 	extract_IMS_ScanReq_Record(IMS_ScanReq_Record *, AGGREGATE) ;
extern int 	extract_IMS_SVecAvail_Record(IMS_SVecAvail_Record *, AGGREGATE) ;
extern int	parse_common_hdr(char *, char *) ;
extern int      ReadLabel_buf(char *, AGGREGATE);
extern void	WriteLabelBuf(char *, AGGREGATE, int) ;

#endif /* _PPSEXTERNODL_ */
