static char *sccs = "@(#)ims_v0Package.c	1.1  12/19/96";
/************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0Package.c
**
** Purpose
**	Functions to handle dynamic packaging.	
**
**	Creator   :   Julie Wang
**
**	Date      :   Aug 19, 1996 
**
************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <string.h>
#include <IK_Network.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_v0.h>

/* prototypes of local functions */
static int collect_package_info (IMS_MSG_STRUCT *, V0_CAT_STRUCT *, V0_PKG *);
static int build_package_groups (IMS_MSG_STRUCT *, V0_CAT_STRUCT *, V0_PKG *);
static int calculate_cost (IMS_MSG_STRUCT *, V0_CAT_STRUCT  *catReq,
													 PKG_DATASET *,  PKG_PROC_OPTION *, PKG_MEDIA_TYPE *,
													 PKG_MEDIA_FORMAT *);
static int cleanup_v0_pkg (IMS_MSG_STRUCT *, V0_PKG *);

/*
** external variables
*/
extern IMS_HASH_STRUCT *v0_package_HashPtr;
extern AGGREGATE pkg_root_tree;

/************************************************************************
**
** v0_package__build_packageHash -
**
** 
**    outputFlag  1 - fill package info into a hash table 
**                2 - write package info to file (not implemented yet) 
**
** return status -
**    IMS_OK      if successful, or possible missing a portion of processing
**                options (in this case, warning message would be posted in
**                the syslog but the server would not be stopped)
**    IMS_FATAL   an indication of errors that requires attention, such as
**                database access error, memory allocation error, etc.
**                V0 server will be stopped in this case.
************************************************************************/
int v0_package__build_packageHash (IMS_MSG_STRUCT *msgDesc,
                                   char *glbLogin,
                                   char *glbPassword,
																	 V0_CAT_STRUCT *catReq,
																	 int outputFlag)
{
	V0_PKG v0_pkg;
	int    status;
		
	v0_pkg.ds_count     = 0;
	v0_pkg.dataset_list = (PKG_DATASET *)NULL; 

	/*
	** collect package information.  It's possible for this routine to result
	** with incomplete supporting information, such as media types without
	** associating media format types.  The list will be pruned later.
	*/
	if ( (status = collect_package_info (msgDesc, catReq, &v0_pkg)) < IMS_OK)
	{
		(void) cleanup_v0_pkg (msgDesc, &v0_pkg);
		return (status);
	}

	/*
	** create package group trees and attach them to the hash table
	*/
	if (v0_pkg.dataset_list == (PKG_DATASET *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_WARNING, 
			">>> v0_package__build_packageHash: no Dynamic Packaging information found.");
		return (IMS_OK);
	}
	else
	{
		status = build_package_groups (msgDesc, catReq, &v0_pkg);
	}

	(void) cleanup_v0_pkg (msgDesc, &v0_pkg);

	if (status < IMS_OK)
	{
		return (status);
	}
	else
	{
		return (IMS_OK);
	}

} /* end of buildHash */

/************************************************************************
**
** collect_package_info -
**
** return: IMS_OK  if successful, or if none system related error ccurred 
**                 when collectiing package info (e.g. missing matching 
**                 media format for a media type, in this case the media type
**                 will not be returned)
**         IMS_FATAL if system related error occurred, such as memory 
**                 allocation errors, or database access errors.
**
************************************************************************/
static int collect_package_info (IMS_MSG_STRUCT *msgDesc,
                                 V0_CAT_STRUCT  *catReq,
                                 V0_PKG         *v0_pkg)
{
	PKG_DATASET      *td_ptr;  
	PKG_PROC_OPTION  *tp_ptr;
	PKG_MEDIA_TYPE   *tm_ptr;
	PKG_MEDIA_FORMAT *tf_ptr;
	V0_VALUE_LIST    *hdv, *tdv;  /* temp ptr to original returned value list */
	V0_VALUE_LIST    *hdr, *tdr;  /* temp ptr to rest of the returned value list*/
	V0_VALUE_LIST    *hpv, *tpv;  /* temp ptr to original returned value list */
	V0_VALUE_LIST    *hpr, *tpr;  /* temp ptr to rest of the returned value list*/
	V0_VALUE_LIST    *hmv, *tmv;  /* temp ptr to original returned value list */
	V0_VALUE_LIST    *hmr, *tmr;  /* temp ptr to rest of the returned value list*/
	V0_VALUE_LIST    *hfv, *tfv;  /* temp ptr to original returned value list */
	V0_VALUE_LIST    *hfr, *tfr;  /* temp ptr to rest of the returned value list*/
	char sql[IMS_COL1024_LEN+1];
	int  rowCount;
	int  status;
	int  first_ds_flag, first_pr_flag, first_mt_flag, first_mf_flag;
	int  proc_count, m_type_count, m_format_count;

	v0_pkg->ds_count = 0;
	v0_pkg->dataset_list = (PKG_DATASET *)NULL;

	/*
	** get a list of IMS visible dataset ids 
	*/
	sql[0] = '\0';
	strcpy (sql, "select distinct dataset, platform, sensor from"
							 " packaging_options where ims_visible_p = 'Y'");

#ifdef QDEBUG
	(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", sql);
#endif 

	catReq->item[0] = (void *)sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( v0_cat (catReq, V0_GETSTR3) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
	       "Database access error occurred when collecting list of dataset"
				 " names packaging_options table."); 
		return (IMS_FATAL);
	}

	hdv = (V0_VALUE_LIST *)catReq->item[2];

	first_ds_flag = 1;
	while (hdv != (V0_VALUE_LIST *)NULL)
	{
		
		tdv = hdv->next_p;

		if (first_ds_flag)
		{
			if ((td_ptr = (PKG_DATASET *)malloc(sizeof(PKG_DATASET))) == NULL)
			{
				(void)ims_msg (msgDesc, IMS_FATAL, 
						"collect_package_info: Memory allocation failed for td_ptr.");

				/* free rest of the result vaules */
				hdr = hdv;
				while (hdr != (V0_VALUE_LIST *)NULL)
				{
					tdr = hdr->next_p;
					free(hdr);
					hdr = tdr;
				}
				return (IMS_FATAL);
			}
			strcpy (td_ptr->dataset_id, hdv->char_value1);
			strcpy (td_ptr->platform, hdv->char_value2);
			strcpy (td_ptr->sensor, hdv->char_value3);
			td_ptr->proc_options_list = (PKG_PROC_OPTION *)NULL;
			td_ptr->next_p = (PKG_DATASET *)NULL;
			v0_pkg->dataset_list = td_ptr;
			first_ds_flag = 0;
		}
		else
		{
			if ((td_ptr->next_p=(PKG_DATASET *)malloc(sizeof(PKG_DATASET))) == NULL)
			{
				(void)ims_msg (msgDesc, IMS_FATAL, 
					"collect_package_info: Memory allocation failed for td_ptr->next_p.");

				/* free rest of the result vaules */
				hdr = hdv;
				while (hdr != (V0_VALUE_LIST *)NULL)
				{
					tdr = hdr->next_p;
					free(hdr);
					hdr = tdr;
				}
				return (IMS_FATAL);
			}
			td_ptr = td_ptr->next_p;

			strcpy (td_ptr->dataset_id, hdv->char_value1);
			strcpy (td_ptr->platform, hdv->char_value2);
			strcpy (td_ptr->sensor, hdv->char_value3);
			td_ptr->proc_options_list = (PKG_PROC_OPTION *)NULL;
			td_ptr->next_p = (PKG_DATASET *)NULL;
		}

		/*
		** find a list of processing options for the current dataset
		**  td->dataset_id
		*/
		sql[0] = '\0';
		sprintf (sql, "select distinct v0_process_type, package_size"
									" from packaging_options"
					 " where dataset = '%s' and platform = '%s' and"
					 " (sensor = '%s' or sensor is null)",
					 td_ptr->dataset_id, td_ptr->platform, td_ptr->sensor);

#ifdef QDEBUG
		(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", sql);
#endif 

		catReq->item[0] = (void *)sql;
		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;

		if ( v0_cat (catReq, V0_GETSTR2) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
       "Database access error occurred when collecting list of processing"
			 " options for dataset %s in packaging_options table.",
			 td_ptr->dataset_id); 

			/* free rest of the result vaules */
			hdr = hdv;
			while (hdr != (V0_VALUE_LIST *)NULL)
			{
				tdr = hdr->next_p;
				free(hdr);
				hdr = tdr;
			}
			return (IMS_FATAL);
		}

		hpv = (V0_VALUE_LIST *)catReq->item[2];
		
		td_ptr->proc_count = 0;
		first_pr_flag = 1;
		while (hpv != (V0_VALUE_LIST *)NULL)
		{
			tpv = hpv->next_p;

			if (first_pr_flag)
			{
				if ((tp_ptr = (PKG_PROC_OPTION *)malloc(sizeof(PKG_DATASET))) == NULL)
				{
					(void)ims_msg (msgDesc, IMS_FATAL, 
						"collect_package_info: Memory allocation failed for tp_ptr.");

					/* free rest of the result vaules */
					hpr = hpv;
					while (hpr != (V0_VALUE_LIST *)NULL)
					{
						tpr = hpr->next_p;
						free(hpr);
						hpr = tpr;
					}

					hdr = hdv;
					while (hdr != (V0_VALUE_LIST *)NULL)
					{
						tdr = hdr->next_p;
						free(hdr);
						hdr = tdr;
					}
	
					return (IMS_FATAL);
				}
				strcpy (tp_ptr->v0_process_type, hpv->char_value1);
				strcpy (tp_ptr->pkg_size, hpv->char_value2);
				tp_ptr->media_type_list = (PKG_MEDIA_TYPE *)NULL;
				tp_ptr->next_p = (PKG_PROC_OPTION *)NULL;

				td_ptr->proc_options_list = tp_ptr;
				first_pr_flag = 0;
			}
			else
			{
				if ((tp_ptr->next_p=(PKG_PROC_OPTION *)malloc
														(sizeof(PKG_PROC_OPTION))) == NULL)
				{
					(void)ims_msg (msgDesc, IMS_FATAL, 
					"collect_package_info: Memory allocation failed for tp_ptr->next_p.");

					/* free rest of the result vaules */
					hpr = hpv;
					while (hpr != (V0_VALUE_LIST *)NULL)
					{
						tpr = hpr->next_p;
						free(hpr);
						hpr = tpr;
					}

					hdr = hdv;
					while (hdr != (V0_VALUE_LIST *)NULL)
					{
						tdr = hdr->next_p;
						free(hdr);
						hdr = tdr;
					}
					return (IMS_FATAL);
				}
				tp_ptr = tp_ptr->next_p;

				strcpy (tp_ptr->v0_process_type, hpv->char_value1);
				strcpy (tp_ptr->pkg_size, hpv->char_value2);
				tp_ptr->media_type_list = (PKG_MEDIA_TYPE *)NULL;
				tp_ptr->next_p = (PKG_PROC_OPTION *)NULL;
			}

			/*
			** find a list of media types for the current process option 
			**  hp->v0_process_type
			*/
			sql[0] = '\0';
			sprintf (sql, "select distinct v0_media_type from packaging_options"
					 " where dataset = '%s' and platform = '%s' and"
					 " (sensor = '%s' or sensor is null)"
					 " and v0_process_type = '%s'",
					 td_ptr->dataset_id, td_ptr->platform, td_ptr->sensor, 
					 tp_ptr->v0_process_type);

#ifdef QDEBUG
			(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", sql);
#endif 

			catReq->item[0] = (void *)sql;
			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;

			if ( v0_cat (catReq, V0_GETSTR) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
	       "Database access error occurred when collecting list of media"
				 " types for dataset %s v0_process_type %s in packaging_options table.",
				 	td_ptr->dataset_id, tp_ptr->v0_process_type); 

				/* free rest of the result vaules */
				hpr = hpv;
				while (hpr != (V0_VALUE_LIST *)NULL)
				{
					tpr = hpr->next_p;
					free(hpr);
					hpr = tpr;
				}

				hdr = hdv;
				while (hdr != (V0_VALUE_LIST *)NULL)
				{
					tdr = hdr->next_p;
					free(hdr);
					hdr = tdr;
				}
				return (IMS_FATAL);
			}

			hmv = (V0_VALUE_LIST *)catReq->item[2];
		
			tp_ptr->mt_count = 0;
			first_mt_flag = 1;
			while (hmv != (V0_VALUE_LIST *)NULL)
			{
				tmv = hmv->next_p;

				if (first_mt_flag)
				{
					if ((tm_ptr = (PKG_MEDIA_TYPE *)malloc
											(sizeof(PKG_MEDIA_TYPE))) == NULL)
					{
						(void)ims_msg (msgDesc, IMS_FATAL, 
						"collect_package_info: Memory allocation failed for tm_ptr.");

						/* free rest of the result vaules */
						hmr = hmv;
						while (hmr != (V0_VALUE_LIST *)NULL)
						{
							tmr = hmr->next_p;
							free(hmr);
							hmr = tmr;
						}

						hpr = hpv;
						while (hpr != (V0_VALUE_LIST *)NULL)
						{
							tpr = hpr->next_p;
							free(hpr);
							hpr = tpr;
						}

						hdr = hdv;
						while (hdr != (V0_VALUE_LIST *)NULL)
						{
							tdr = hdr->next_p;
							free(hdr);
							hdr = tdr;
						}
		
						return (IMS_FATAL);
					}
					strcpy (tm_ptr->v0_media_type, hmv->char_value1);
					tm_ptr->media_fmt_list = (PKG_MEDIA_FORMAT *)NULL;
					tm_ptr->next_p = (PKG_MEDIA_TYPE *)NULL;

					tp_ptr->media_type_list = tm_ptr;
					first_mt_flag = 0;
				}
				else
				{
					if ((tm_ptr->next_p=(PKG_MEDIA_TYPE *)
															malloc(sizeof(PKG_MEDIA_TYPE))) == NULL)
					{
						(void)ims_msg (msgDesc, IMS_FATAL, 
						"collect_package_info: Memory allocation failed for"
						" tm_ptr->next_p.");

						/* free rest of the result vaules */
						hmr = hmv;
						while (hmr != (V0_VALUE_LIST *)NULL)
						{
							tmr = hmr->next_p;
							free(hmr);
							hmr = tmr;
						}

						hpr = hpv;
						while (hpr != (V0_VALUE_LIST *)NULL)
						{
							tpr = hpr->next_p;
							free(hpr);
							hpr = tpr;
						}

						hdr = hdv;
						while (hdr != (V0_VALUE_LIST *)NULL)
						{
							tdr = hdr->next_p;
							free(hdr);
							hdr = tdr;
						}
						return (IMS_FATAL);
					}
					tm_ptr = tm_ptr->next_p;

					strcpy (tm_ptr->v0_media_type, hmv->char_value1);
					tm_ptr->media_fmt_list = (PKG_MEDIA_FORMAT *)NULL;
					tm_ptr->next_p = (PKG_MEDIA_TYPE *)NULL;
				}

				/*
				** find a list of media formats for the current media type 
				**  hm->v0_media_type
				*/
				sql[0] = '\0';
				sprintf (sql, 
						 "select distinct v0_media_fmt_type"
						 " from packaging_options"
						 " where dataset = '%s' and platform = '%s' and"
						 " (sensor = '%s' or sensor is null)"
						 " and v0_process_type = '%s' and v0_media_type = '%s'",
						 td_ptr->dataset_id, td_ptr->platform, td_ptr->sensor, 
						 tp_ptr->v0_process_type, tm_ptr->v0_media_type);

#ifdef QDEBUG
				(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", sql);
#endif 

				catReq->item[0] = (void *)sql;
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;

				if ( v0_cat (catReq, V0_GETSTR) < IMS_OK)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
	       "Database access error occurred when collecting list of media"
				 " format types for dataset %s v0_process_type %s v0_media_type %s"
				 " in packaging_options table.",
				 		td_ptr->dataset_id, tp_ptr->v0_process_type, tm_ptr->v0_media_type); 

					/* free rest of the result vaules */
					hmr = hmv;
					while (hmr != (V0_VALUE_LIST *)NULL)
					{
						tmr = hmr->next_p;
						free(hmr);
						hmr = tmr;
					}

					hpr = hpv;
					while (hpr != (V0_VALUE_LIST *)NULL)
					{
						tpr = hpr->next_p;
						free(hpr);
						hpr = tpr;
					}

					hdr = hdv;
					while (hdr != (V0_VALUE_LIST *)NULL)
					{
						tdr = hdr->next_p;
						free(hdr);
						hdr = tdr;
					}
					return (IMS_FATAL);
				}

				hfv = (V0_VALUE_LIST *)catReq->item[2];

				tm_ptr->mf_count = 0;
				first_mf_flag = 1;
				while (hfv != (V0_VALUE_LIST *)NULL )
				{
					tfv = hfv->next_p;

					if (first_mf_flag)
					{
						if ((tf_ptr = (PKG_MEDIA_FORMAT *)malloc
											(sizeof(PKG_MEDIA_FORMAT))) == NULL)
						{
							(void)ims_msg (msgDesc, IMS_FATAL, 
							"collect_package_info: Memory allocation failed for tf_ptr.");

							/* free rest of the result vaules */
							hfr = hfv;
							while (hfr != (V0_VALUE_LIST *)NULL)
							{
								tfr = hfr->next_p;
								free(hfr);
								hfr = tfr;
							}

							hmr = hmv;
							while (hmr != (V0_VALUE_LIST *)NULL)
							{
								tmr = hmr->next_p;
								free(hmr);
								hmr = tmr;
							}

							hpr = hpv;
							while (hpr != (V0_VALUE_LIST *)NULL)
							{
								tpr = hpr->next_p;
								free(hpr);
								hpr = tpr;
							}

							hdr = hdv;
							while (hdr != (V0_VALUE_LIST *)NULL)
							{
								tdr = hdr->next_p;
								free(hdr);
								hdr = tdr;
							}
		
							return (IMS_FATAL);
						}
	 					strcpy (tf_ptr->v0_media_fmt, hfv->char_value1);

						if ( status = calculate_cost (msgDesc, catReq, td_ptr, tp_ptr, 
																					 tm_ptr, tf_ptr) < IMS_OK)
						{
							/* free rest of the result vaules */
							hfr = hfv;
							while (hfr != (V0_VALUE_LIST *)NULL)
							{
								tfr = hfr->next_p;
								free(hfr);
								hfr = tfr;
							}

							hmr = hmv;
							while (hmr != (V0_VALUE_LIST *)NULL)
							{
								tmr = hmr->next_p;
								free(hmr);
								hmr = tmr;
							}

							hpr = hpv;
							while (hpr != (V0_VALUE_LIST *)NULL)
							{
								tpr = hpr->next_p;
								free(hpr);
								hpr = tpr;
							}

							hdr = hdv;
							while (hdr != (V0_VALUE_LIST *)NULL)
							{
								tdr = hdr->next_p;
								free(hdr);
								hdr = tdr;
							}
		
							return(status);
						}

						tf_ptr->next_p = (PKG_MEDIA_FORMAT *)NULL;

						tm_ptr->media_fmt_list = tf_ptr;
						first_mf_flag = 0;
					}
					else
					{
						if ((tf_ptr->next_p=(PKG_MEDIA_FORMAT *)malloc
															(sizeof(PKG_MEDIA_FORMAT))) == NULL)
						{
							(void)ims_msg (msgDesc, IMS_FATAL, 
							"collect_package_info: Memory allocation failed for"
							" tf_ptr->next_p.");

							/* free rest of the result vaules */
							hfr = hfv;
							while (hfr != (V0_VALUE_LIST *)NULL)
							{
								tfr = hfr->next_p;
								free(hfr);
								hfr = tfr;
							}

							hmr = hmv;
							while (hmr != (V0_VALUE_LIST *)NULL)
							{
								tmr = hmr->next_p;
								free(hmr);
								hmr = tmr;
							}

							hpr = hpv;
							while (hpr != (V0_VALUE_LIST *)NULL)
							{
								tpr = hpr->next_p;
								free(hpr);
								hpr = tpr;
							}

							hdr = hdv;
							while (hdr != (V0_VALUE_LIST *)NULL)
							{
								tdr = hdr->next_p;
								free(hdr);
								hdr = tdr;
							}
							return (IMS_FATAL);
						}
						tf_ptr = tf_ptr->next_p;

						strcpy (tf_ptr->v0_media_fmt, hfv->char_value1);

						if ( status = calculate_cost (msgDesc, catReq, td_ptr, tp_ptr, 
																	 tm_ptr, tf_ptr) < IMS_OK)
						{
							/* free rest of the result vaules */
							hfr = hfv;
							while (hfr != (V0_VALUE_LIST *)NULL)
							{
								tfr = hfr->next_p;
								free(hfr);
								hfr = tfr;
							}

							hmr = hmv;
							while (hmr != (V0_VALUE_LIST *)NULL)
							{
								tmr = hmr->next_p;
								free(hmr);
								hmr = tmr;
							}

							hpr = hpv;
							while (hpr != (V0_VALUE_LIST *)NULL)
							{
								tpr = hpr->next_p;
								free(hpr);
								hpr = tpr;
							}

							hdr = hdv;
							while (hdr != (V0_VALUE_LIST *)NULL)
							{
								tdr = hdr->next_p;
								free(hdr);
								hdr = tdr;
							}
		
							return(status);
						}

						tf_ptr->next_p = (PKG_MEDIA_FORMAT *)NULL;
					}
					free (hfv);
					hfv = tfv;

					tm_ptr->mf_count++;
				} /* media formats loop within a media type */
				free (hmv);
				hmv = tmv;

				tp_ptr->mt_count++;
			} /* media types loop within a process option */

			free (hpv); 
			hpv = tpv;

			td_ptr->proc_count++;
		} /* process options loop within a dataset */

		free (hdv);
		hdv = tdv;

		v0_pkg->ds_count++;
	} /* dataset loop */

	return (IMS_OK);
} /* end of collect_package_info */


/************************************************************************
**
** build_package_groups - create odl PACKAGE groups and subgroups and
**      insert each group into the hash table
**
** return   IMS_OK    if successful
**          IMS_FATAL if error occurred.  In this case, the package hash
**            will be discarded.
**
************************************************************************/
static int build_package_groups (IMS_MSG_STRUCT *msgDesc, 
                                 V0_CAT_STRUCT *catReq, 
                                 V0_PKG *v0_pkg)
{
	PKG_DATASET      *td;
	PKG_PROC_OPTION  *tp;
	PKG_MEDIA_TYPE   *tm;
	PKG_MEDIA_FORMAT *tf;
	IMS_HASH_ENTRY   item;
	IMS_HASH_ENTRY   *hashPtr;
	AGGREGATE        dgroup, pgroup, mtgroup, mfgroup;
	AGGREGATE        group;
	ATTRIBUTE        parameter;
	VALUE_DATA       value;
	VALUE            value_ptr;
	char             tmpStr[IMS_COL80_LEN+1];

	/*
	** initialize a root tree for all PAKCAGE groups
	*/
	if ((pkg_root_tree = NewAggregate (NULL, KA_GROUP, "root", NULL)) == 
		(AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"build_pakckage_groups: failed to create root aggregate for all"
			" package groups.");
		return (IMS_FATAL);
	}

	td = v0_pkg->dataset_list;
	while (td != (PKG_DATASET* )NULL)
	{

		/*
		** create a PACKAGE group
		*/
		if ( (dgroup = NewAggregate 
			(pkg_root_tree, KA_GROUP, "PACKAGE", NULL)) == (AGGREGATE)NULL)
		{
			(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize a PACKAGE group");
			goto ERROR;
		}

		if ( (parameter = NewParameter 
			(dgroup, KP_ATTRIBUTE, "DATA_CENTER_ID")) == (PARAMETER)NULL)
		{
			(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize DATA_CENTER_ID parameter. ");
			goto ERROR;
		}
		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString ("ASF", 3);
		NewValue (parameter, &value);
		            
		if ( (parameter = NewParameter 
			(dgroup, KP_ATTRIBUTE, "DATASET_ID")) == (PARAMETER)NULL)
		{
			(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize DATASET_ID parameter. ");
			goto ERROR;
		}
		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (td->dataset_id, strlen(td->dataset_id));
		NewValue (parameter, &value);
		            
		if ( (parameter = NewParameter 
			(dgroup, KP_ATTRIBUTE, "PACKAGE_ID")) == (PARAMETER)NULL)
		{
			(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize PACKAGE_ID parameter. ");
			goto ERROR;
		}
		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString ("*", 1);
		NewValue (parameter, &value);
		            
		if ( (parameter = NewParameter 
			(dgroup, KP_ATTRIBUTE, "COMMENT")) == (PARAMETER)NULL)
		{
			(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize COMMENT parameter. ");
			goto ERROR;
		}
		parameter->value_kind = KV_SCALAR;
		tmpStr[0] = '\0';
		strcpy (tmpStr, "");
		value = ODLConvertString (tmpStr, strlen(tmpStr));
		NewValue (parameter, &value);
		            
		if ( (parameter = NewParameter 
			(dgroup, KP_ATTRIBUTE, "INFO_PROMPT")) == (PARAMETER)NULL)
		{
			(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize INFO_PROMPT parameter. ");
			goto ERROR;
		}
		parameter->value_kind = KV_SCALAR;
		tmpStr[0] = '\0';
		strcpy (tmpStr, "N/A");
		value = ODLConvertString (tmpStr, strlen(tmpStr));
		NewValue (parameter, &value);
		            
		if ((parameter=NewParameter
			(dgroup, KP_ATTRIBUTE, "NUMBER_OF_GRANULES")) == (PARAMETER)NULL)
		{
			(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize"
				" NUMBER_OF_GRANULES parameter. ");
			goto ERROR;
		}
		parameter->value_kind = KV_SCALAR;
		tmpStr[0] = '\0';
		strcpy (tmpStr, "1");
		value = ODLConvertInteger (tmpStr, strlen(tmpStr));
		NewValue (parameter, &value);

		/*
		** fill in count for Processing Options
		*/
		if ((parameter=NewParameter
				 (dgroup, KP_ATTRIBUTE, "NUMBER_OF_OPTIONS"))==(PARAMETER)NULL)
		{
			(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize"
				" NUMBER_OF_OPTIONS parameter. "); 
			goto ERROR;
		}
		parameter->value_kind = KV_SCALAR;
		tmpStr[0] = '\0';
		sprintf (tmpStr, "%d", td->proc_count);
		value = ODLConvertInteger (tmpStr, strlen(tmpStr));
		NewValue (parameter, &value);

		tp = td->proc_options_list;

		while (tp != (PKG_PROC_OPTION *)NULL)
		{
			/*
			** create a PROCESSING_OPTION group
			*/
			if ( (pgroup = NewAggregate 
				(dgroup, KA_GROUP, "PROCESSING_OPTION", NULL)) == (AGGREGATE)NULL)
			{
				(void)ims_msg (msgDesc, IMS_FATAL, 
					"build_package_groups: Failed to initilize a PROCESSING_OPTION group");
				goto ERROR;
			}
		            
			if ( (parameter = NewParameter 
				(pgroup, KP_ATTRIBUTE, "OPTION_ID")) == (PARAMETER)NULL)
			{
				(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize OPTION_ID parameter. ");
				goto ERROR;
			}
			parameter->value_kind = KV_SCALAR;
			value = ODLConvertString (tp->v0_process_type, 
																strlen(tp->v0_process_type));
			NewValue (parameter, &value);
		            
			if ( (parameter = NewParameter 
				(pgroup, KP_ATTRIBUTE, "PACKAGE_SIZE")) == (PARAMETER)NULL)
			{
				(void)ims_msg (msgDesc, IMS_FATAL, 
				"build_package_groups: Failed to initilize PACKAGE_SIZE parameter. ");
				goto ERROR;
			}
			parameter->value_kind = KV_SCALAR;
			value = ODLConvertString (tp->pkg_size, strlen(tp->pkg_size));
			NewValue (parameter, &value);
		            
			if ((parameter=NewParameter
				 	(pgroup, KP_ATTRIBUTE, "NUMBER_OF_MEDIA_TYPE"))==(PARAMETER)NULL)
			{
				(void)ims_msg (msgDesc, IMS_FATAL, 
					"build_package_groups: Failed to initilize"
					" NUMBER_OF_MEDIA_TYPE parameter. "); 
				goto ERROR;
			}
			parameter->value_kind = KV_SCALAR;
			tmpStr[0] = '\0';
			sprintf (tmpStr, "%d", tp->mt_count);
			value = ODLConvertInteger (tmpStr, strlen(tmpStr));
			NewValue (parameter, &value);

			tm = tp->media_type_list;

			while (tm != (PKG_MEDIA_TYPE *)NULL)
			{
				/*
				** create a MEDIA_TYPE group
				*/
				if ( (mtgroup = NewAggregate 
					(pgroup, KA_GROUP, "MEDIA_TYPE", NULL)) == (AGGREGATE)NULL)
				{
					(void)ims_msg (msgDesc, IMS_FATAL, 
					"build_package_groups: Failed to initilize a MEDIA_TYPE group");
					goto ERROR;
				}
		            
				if ( (parameter = NewParameter 
					(mtgroup, KP_ATTRIBUTE, "TYPE_ID")) == (PARAMETER)NULL)
				{
					(void)ims_msg (msgDesc, IMS_FATAL, 
					"build_package_groups: Failed to initilize TYPE_ID parameter. ");
					goto ERROR;
				}
				parameter->value_kind = KV_SCALAR;
				value = ODLConvertString (tm->v0_media_type, 
																strlen(tm->v0_media_type));
				NewValue (parameter, &value);
		            
				if ((parameter=NewParameter
				 		(mtgroup, KP_ATTRIBUTE, "NUMBER_OF_MEDIA_FORMAT"))==(PARAMETER)NULL)
				{
					(void)ims_msg (msgDesc, IMS_FATAL, 
						"build_package_groups: Failed to initilize"
						" NUMBER_OF_MEDIA_FORMAT parameter. "); 
					goto ERROR;
				}
				parameter->value_kind = KV_SCALAR;
				tmpStr[0] = '\0';
				sprintf (tmpStr, "%d", tm->mf_count);
				value = ODLConvertInteger (tmpStr, strlen(tmpStr));
				NewValue (parameter, &value);

				tf = tm->media_fmt_list;

				while (tf != (PKG_MEDIA_FORMAT *)NULL)
				{
					/*
					** create a MEDIA_FORMAT group
					*/
					if ( (mfgroup = NewAggregate 
						(mtgroup, KA_GROUP, "MEDIA_FORMAT", NULL)) == (AGGREGATE)NULL)
					{
						(void)ims_msg (msgDesc, IMS_FATAL, 
						"build_package_groups: Failed to initilize a MEDIA_FORMAT group");
						goto ERROR;
					}
		            
					if ( (parameter = NewParameter 
						(mfgroup, KP_ATTRIBUTE, "FORMAT_ID")) == (PARAMETER)NULL)
					{
						(void)ims_msg (msgDesc, IMS_FATAL, 
						"build_package_groups: Failed to initilize FORMAT_ID parameter. ");
						goto ERROR;
					}
					parameter->value_kind = KV_SCALAR;
					value = ODLConvertString (tf->v0_media_fmt, 
																strlen(tf->v0_media_fmt));
					NewValue (parameter, &value);

					if ( (parameter = NewParameter 
						(mfgroup, KP_ATTRIBUTE, "APPROX_COST")) == (PARAMETER)NULL )
					{
						(void) ims_msg (msgDesc, IMS_FATAL,
						"build_package_groups: Failed to initilize APPROX_COST parameter.");
						goto ERROR;	
					}
					parameter->value_kind = KV_SCALAR;
					tmpStr[0] = '\0';
					(void) sprintf(tmpStr, "%7.2f", tf->cost);
					value = ODLConvertReal (tmpStr, strlen(tmpStr) );
					NewValue (parameter, &value);

					tf = tf->next_p;
				}

				tm = tm->next_p;
			}

			tp = tp->next_p;
		} /* loop of processing options */

		td = td->next_p;
	} /* loop of dataset ids */

	/* 
	** initialize a hash table
	*/
	v0_package_HashPtr = (IMS_HASH_STRUCT *) NULL;
	if ( (v0_package_HashPtr =
	        ims_hashCreate (v0_pkg->ds_count, IMS_STRING, msgDesc)) ==
	            (IMS_HASH_STRUCT *) NULL)
	{
		ims_msg (msgDesc, IMS_FATAL, 
			"build_pakckage_groups: Failed to create package hash table.");
		goto ERROR;															
	}

	if ( (item.data = FindAggregate (pkg_root_tree, "PACKAGE")) != NULL)
	{
		parameter = (ATTRIBUTE)NULL;

		if (((parameter=FindParameter((AGGREGATE)item.data,"DATASET_ID"))!=NULL) && 
	       ( ((value_ptr=FirstValue(parameter))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
		{
			(void) v0_process__prepare_string (value_ptr->item.value.string);
			item.key = (char *)value_ptr->item.value.string;

			if ((hashPtr = ims_hashSearch
			   	(v0_package_HashPtr, &item, IMS_ENTER, msgDesc))
					==(IMS_HASH_ENTRY*)NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
			   "build_package_groups: Failed to insert an entry"
				 " into the package hash table. dataset id %s", 
					value_ptr->item.value.string);
				goto ERROR;
			
			}
			else
			{
#ifdef DEBUG 
				(void)ims_msg (msgDesc, IMS_INFO, 
					"** %s package info has been inserted to the hash table.", 
					value_ptr->item.value.string);
#endif 
			}
		}
	}

	while ( (group = FindNextAggregate 
					 (pkg_root_tree, item.data, "PACKAGE")) != (AGGREGATE)NULL)
	{
		item.data = group;
		parameter = (ATTRIBUTE)NULL;

		if (((parameter=FindParameter((AGGREGATE)item.data,"DATASET_ID"))!=NULL)&&
	      ( ((value_ptr=FirstValue(parameter))!=NULL) &&
	      (value_ptr->item.length>0) ) ) 
		{
			(void) v0_process__prepare_string (value_ptr->item.value.string);
			item.key = (char *)value_ptr->item.value.string;

			if ((hashPtr = ims_hashSearch
				(v0_package_HashPtr, &item, IMS_ENTER, msgDesc))
				==(IMS_HASH_ENTRY*)NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
			  	"Failed to insert an entry"
			 	  " into the package hash table. dataset id %s", 
					value_ptr->item.value.string);
				goto ERROR;
			}
			else
			{
#ifdef DEBUG
				(void)ims_msg (msgDesc, IMS_INFO, 
						"** %s package info was inserted to the hash table.", 
				    value_ptr->item.value.string);
#endif
			}
		}

	}

	return (IMS_OK);

	ERROR:
		/*
		** free the entire package root tree
		*/
		(void) v0_msgTree__destroy (pkg_root_tree);
		pkg_root_tree = (AGGREGATE)NULL;

		/*
		** clean up the Hash table if needed
		*/
		if (v0_package_HashPtr != NULL)
		{
			ims_hashDestroy (v0_package_HashPtr, msgDesc);
			v0_package_HashPtr = (IMS_HASH_STRUCT *) NULL;
		}

		return (IMS_FATAL);

} /* end of build_package_group */

/************************************************************************
**
** cleanup_v0_pkg -
**
************************************************************************/
static int cleanup_v0_pkg (IMS_MSG_STRUCT *msgDesc,
                           V0_PKG         *v0_pkg)
{
	PKG_PROC_OPTION  *hp, *tp;
	PKG_MEDIA_TYPE   *hm, *tm;
	PKG_MEDIA_FORMAT *hf, *tf;
	PKG_DATASET      *hd, *td, *temp;


	if (v0_pkg->dataset_list == (PKG_DATASET *)NULL)
	{
		return (IMS_OK);
	}
	
	hd = v0_pkg->dataset_list;

	while (hd != (PKG_DATASET *)NULL)
	{
		td = hd->next_p;
		hp = hd->proc_options_list;

		while ( hp != (PKG_PROC_OPTION  *)NULL)
		{
			tp = hp->next_p;
			hm = hp->media_type_list;

			while ( hm != (PKG_MEDIA_TYPE *)NULL)
			{
				tm = hm->next_p;
				hf = hm->media_fmt_list;
	
				while ( hf != (PKG_MEDIA_FORMAT *)NULL)
				{
					tf = hf->next_p;
					free (hf);
					hf = tf;
				}
	
				free (hm);
				hm = tm;
			}
	
			free (hp);
			hp = tp;
		}
		free (hd);
		hd = td;
	}

	return (IMS_OK);

} /* end of cleanup_v0_pkg */


/************************************************************************
**
** calculate_cost -
**
************************************************************************/
static int calculate_cost (IMS_MSG_STRUCT *msgDesc,
                           V0_CAT_STRUCT  *catReq,
                           PKG_DATASET      *td_ptr,  
                           PKG_PROC_OPTION  *tp_ptr,
                           PKG_MEDIA_TYPE   *tm_ptr,
                           PKG_MEDIA_FORMAT *tf_ptr)
{
	V0_VALUE_LIST    *temp_ptr; 
	int  media_type, media_fmt_type;
	char sql[IMS_COL1024_LEN+1];
	int  rowCount;

	/*
	** find the cost for the current combination of platform, sensor
	** dataset, process_type, media_type, and media_format
	*/
	sql[0] = '\0';
	sprintf (sql, "select media_type from media_type_map where"
								" v0_media_type = '%s'",
	         tm_ptr->v0_media_type);

#ifdef QDEBUG
	(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", sql);
#endif 

	catReq->item[0] = (void *)sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( v0_cat (catReq, V0_GETSMALLINT) < IMS_OK )
	{
		(void)ims_msg (msgDesc, IMS_FATAL,
			"calculate_cost: Failed to find media_type in media_type_map table"
			" for v0 media type %s",
			tm_ptr->v0_media_type);
		return (IMS_FATAL);
	}

	temp_ptr = (V0_VALUE_LIST *)catReq->item[2];
	media_type = temp_ptr->smallint_value;
	free(temp_ptr);

	sprintf (sql, "select media_fmt_type from media_fmt_map where"
								" v0_media_fmt_type = '%s'",
	         tf_ptr->v0_media_fmt);

#ifdef QDEBUG
				(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", sql);
#endif 

	catReq->item[0] = (void *)sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if (v0_cat (catReq, V0_GETSMALLINT) < IMS_OK)
	{
		(void)ims_msg (msgDesc, IMS_FATAL,
			"calculate_cost: Failed to find media_fmt_type in media_fmt_map table"
			" for v0 media format %s",
			tf_ptr->v0_media_fmt);
		return (IMS_FATAL);
	}

	temp_ptr = (V0_VALUE_LIST *)catReq->item[2];
	media_fmt_type = temp_ptr->smallint_value;
	free(temp_ptr);

	if (td_ptr->sensor[0] == '\0')
	{
		sprintf (sql, 
				 "exec v0_get_item_cost '%s', null, '%s' ,'%s', %d, %d, 1",
	         td_ptr->platform, td_ptr->dataset_id, 
	         tp_ptr->v0_process_type, media_type, media_fmt_type);
	}
	else
	{
		sprintf (sql, 
				 "exec v0_get_item_cost '%s', '%s', '%s' ,'%s', %d, %d, 1",
	         td_ptr->platform, td_ptr->sensor, td_ptr->dataset_id, 
	         tp_ptr->v0_process_type, media_type, media_fmt_type);
	}

#ifdef QDEBUG
	(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", sql);
#endif 

	catReq->item[0] = (void *)sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if (v0_cat (catReq, V0_GETLINEITEMCOST) < IMS_OK)
	{
		(void)ims_msg (msgDesc, IMS_FATAL,
			"calculate_cost: Failed to calculate packaging option cost.");
		return (IMS_FATAL);
	}

	temp_ptr = (V0_VALUE_LIST *)catReq->item[2];
	tf_ptr->cost = temp_ptr->real_value;
	free(temp_ptr);

	return (IMS_OK);

} /* end of calculate_cost */
