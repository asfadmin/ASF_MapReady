static char *sccs = "@(#)ims_v0Query.c	5.7  04/18/97";
/********************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0Query.c
**
** Purpose
**		ims_v0Query creates SYBASE query statements to retrieve results
**		for various types of search requests.
**
**	Creator   :   Julie Wang
**
**	Date      :   July 14, 1994
**
** Modifications:
**
**   04/18/97    jwang   Granule id search. 
**   
**   07/24/96    jwang   Process_level datatype change.
**   
**   04/25/96    jwang   Removed the query generation function for 
**                       Parameter querying. 
**
**   02/20/96    jwang   R1Bprime (preliminary).
**
**   09/05/95    jwang   Used new time fields in select clauses.
**
**   06/05/95    jwang   Changed all reference of cdb_ or CDB_ to 
**                       ims_ or IMS_
**
**   06/05/95    jwang   IMS front-end order processing
**
**   03/31/95    jwang   Spatial search pass 2 
**
**   02/01/95    jwang   Spatial search pass 1
**
**   11/18/94    jwang   Added seasonal search capability.
**
**   10/01/94    jwang   IMS/DADS Release 0.
**
********************************************************************/

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
#include <ims_archive.h>
#include <ims_v0.h>

#define COMMA   ", "
#define AND     " and "

static int check_length (V0_DESC_STRUCT *, int *, int, int);

/*
**  General format for a query statment is:
**
**     select  <select_list>
**     from <table_list>
**     where <where_clause>
*/

/***********************************************************************
**
** v0_query__build_ds -
**
** purpose: creates the query string for dataset list retrival 
**
** called by: v0_process__retrieve_ds_list
**
** return    IMS_OK  successful
**           IMS_FATAL system error
**
************************************************************************/
int v0_query__build_ds (V0_DESC_STRUCT *v0Desc)
{
	V0_VALUE_LIST  *t_ptr; /* ptr to track the value list elements */
	V0_REQUEST_STRUCT request;
	char *select;
	char *from;
	char *where;
	char *sql;
	int  where_init;
	int  list_init;      /* set to 1 when the first element of a value 
	                        list is found */ 
	int  spatial_flag;
	int  where_count;    /* current word count of the where clause, this
													is checked whenever it's increased to make sure
													it does not exceed the maximum length */


	/* 
	** Initialize local variables 
	*/
	select = v0Desc->query.select;
	from = v0Desc->query.from;
	where = v0Desc->query.where;
	sql = v0Desc->query.sql;

	request      = v0Desc->request;

	where_init   = 0;
	list_init    = 0;
	spatial_flag = 0;
	where_count  = 0;

	/*
	**  Begin constructing the query statment.
	**
	**  build select_list  -- 
	**
	**  The format for a select_list is:
	**
	**   select distinct <table_name>.<field_name>[,<table_name>.<field_name> ...]
	**
	*/
	strcpy (select, " distinct d.dataset_idx, d.dataset, d.md_id, d.sensor, d.platform, dp.day_night, dp.process_level, dp.granules_table, dp.temporal_type, dp.spatial_type, dp.campaign");
	select = select + strlen(select);


	/* 
	** build table_list  --
	**
	** The format for a table_list is:
	**
	**   from <table_name>[,<table_name> ...]
	**
	*/
	strcpy (from, " dataset_relation d, dataset_policy dp");
	from = from + strlen (from);

	/*
	** append table name parameter if any parameter values are specified.
	*/
	if (request.parameter != (V0_VALUE_LIST *)NULL )
	{
		strcpy (from, COMMA);
		from = from + strlen(from);
		strcpy (from, " dataset_parameter param");
		from = from + strlen(from);
	}

	/*
	** where clause --
	**
	** The format for a where clause is:
	**
	**   where (<join_list>)  
	**       and ( <keyword>=<value>|<keyword> in (<value_list>) 
	**       [and <keyword>=<value>|<keyword> in (<value_list>) ...])
	**
	** The format for a value_list is:
	**
	**   "value1" [and "value2"]
	**
	** Two flag will be used in constructing the where clause:
	**
	**  'where_int' is set after the first keyword search spec been added
	**  to the where clause.  An 'and' will be inserted whenever a new 
	**  keyword spec is added.
	**    
	**  'list_init' is set after the first value been added into the 
	**  value_list. An 'and' will be inserted before whenever a new 
	**  value_list item is added for the keyword.
	**
	*/

	/*
	**  We'll build the join_list first - 
	*/

	strcpy (where, " (");
	if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
	{
		return (IMS_ERROR);
	}
	where = where + strlen(where);
	strcpy (where, 
		"dp.ims_visible_p = 'Y'"
		" and d.dataset_idx = dp.dataset_idx");
	if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
	{
		return (IMS_ERROR);
	}
	where = where + strlen (where);



	/*
	** add joins for parameter only if any parameter values are specified
	*/
	if (request.parameter != (V0_VALUE_LIST *)NULL )
	{
		strcpy (where, AND); 
		if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);
		strcpy (where, " d.dataset_idx = param.dataset_idx");
		if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);
	}

	strcpy (where, " )");
	if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
	{
		return (IMS_ERROR);
	}
	where = where + strlen(where);

	/*
	** make sure temporal type and spatial_type are present if this is 
	** an inventory search. Otherwise, it's not possible to continue
	*/
	if (v0Desc->RxType == V0_INVENTORY_SEARCH )
	{
		strcpy (where, AND); 
		if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);
		strcpy (where, 
			" (dp.temporal_type is not null and dp.spatial_type is not null"
			" and d.md_id is not null)");
		if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);
	}

	/* 
	** Begin <keyword>=<value> or <keyword> in <value_list> condition 
	** query.
	**
	*/

	if ( (request.dataset_id != (V0_VALUE_LIST *)NULL) ||
	     (request.sensor != (V0_VALUE_LIST *)NULL ) ||
	     (request.platform != (V0_VALUE_LIST *)NULL ) ||
	     (request.parameter != (V0_VALUE_LIST *)NULL ) ||
	     (request.campaign != (V0_VALUE_LIST *)NULL )       ||
	     (request.process_level != (V0_VALUE_LIST *)NULL  )  ||
	     (request.global_granules_p == 'Y')  ||
	     (request.browse_p[0] != '\0')       ||
	     (request.start_time[0] != '\0')     ||
	     (request.end_time[0] != '\0')       ||
	     (request.day_night[0] != '\0'))       
	{
		strcpy (where, AND); 
		if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);


		/* 
		** dataset_id condition
		*/
		if (request.dataset_id != (V0_VALUE_LIST *)NULL)
		{
	
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
			
			list_init = 0;
			strcpy (where, " d.dataset in (");
			if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
	
			t_ptr = request.dataset_id;
	
			while (t_ptr != (V0_VALUE_LIST *)NULL )
			{
	
				if (list_init)
				{
					strcpy (where, COMMA);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
				}
				else
				{
					list_init = 1;
				}
	
				(void) sprintf (where, " \"%s\"", t_ptr->char_value1);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen (where);
			
				t_ptr = t_ptr->next_p;
			}
	
			strcpy (where, ")");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen (where);

		} /* dataset_id */

		/*
		** sensor condition
		*/
	
		if (request.sensor != (V0_VALUE_LIST *)NULL )
		{
	
		
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
	
			list_init = 0;
			strcpy (where, " d.sensor in (");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
	
			t_ptr = request.sensor;
	
			while (t_ptr != (V0_VALUE_LIST *)NULL )
			{
	
				if (list_init)
				{
					strcpy (where, COMMA);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
				}
				else
				{
					list_init = 1;
				}
	
				(void) sprintf (where, " \"%s\"", t_ptr->char_value1);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen (where);
			
				t_ptr = t_ptr->next_p;
			}
	
			strcpy (where, ")");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen (where);
	
	
		}  /* sensor */ 
	
	
		/*
		** platform condition
		*/
	
		if (request.platform != (V0_VALUE_LIST *)NULL )
		{
	
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
			
			list_init = 0;
			strcpy (where, " d.platform in (");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
	
			t_ptr = request.platform;
	
			while (t_ptr != (V0_VALUE_LIST *)NULL )
			{
	
				if (list_init)
				{
					strcpy (where, COMMA);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
				}
				else
				{
					list_init = 1;
				}
	
				(void) sprintf (where, " \"%s\"", t_ptr->char_value1);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen (where);
				
				t_ptr = t_ptr->next_p;
			}
	
			strcpy (where, ")");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen (where);
	
		}  /* platform */ 
	
		/*
		** parameter condition 
		*/
	
		if (request.parameter != (V0_VALUE_LIST *)NULL )
		{
	
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
			
			list_init = 0;
			strcpy (where, " param.parameter in (");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
	
			t_ptr = request.parameter;
	
			while (t_ptr != (V0_VALUE_LIST *)NULL )
			{
	
				if (list_init)
				{
					strcpy (where, COMMA);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
				}
				else
				{
					list_init = 1;
				}
	
				(void) sprintf (where, " \"%s\"", t_ptr->char_value1);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen (where);
			
				t_ptr = t_ptr->next_p;
			}
	
			strcpy (where, ")");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen (where);
	
		}  /* parameter */ 

		/*
		** campaign condition 
		*/
	
		if (request.campaign != (V0_VALUE_LIST *)NULL )
		{
	
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
			
			list_init = 0;
			strcpy (where, " dp.campaign in (");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
	
			t_ptr = request.campaign;
	
			while (t_ptr != (V0_VALUE_LIST *)NULL )
			{
	
				if (list_init)
				{
					strcpy (where, COMMA);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
				}
				else
				{
					list_init = 1;
				}
	
				(void) sprintf (where, " \"%s\"", t_ptr->char_value1);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen (where);
			
				t_ptr = t_ptr->next_p;
			}
	
			strcpy (where, ")");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen (where);
	
		}  /* campaign */ 

		/*
		** process_level condition 
		*/
	
		if (request.process_level != (V0_VALUE_LIST *)NULL )
		{
	
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
			
			list_init = 0;
			strcpy (where, " dp.process_level in (");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
	
			t_ptr = request.process_level;
	
			while (t_ptr != (V0_VALUE_LIST *)NULL )
			{
	
				if (list_init)
				{
					strcpy (where, COMMA);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
				}
				else
				{
					list_init = 1;
				}
	
				(void) sprintf (where, " %d", t_ptr->smallint_value);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen (where);
			
				t_ptr = t_ptr->next_p;
			}
	
			strcpy (where, ")");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen (where);
	
		}  /* process_level */ 
	
		/*
		** global_granules_p condition
		*/
	
		if (request.global_granules_p == 'Y')
		{
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
	
			strcpy (where, " dp.global_granules_p = 'Y'");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
		} /* global_granules_p */
	
		/*
		** browse_p condition
		*/
		if (request.browse_p[0] != '\0')
		{
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
	
			(void) sprintf (where, "dp.browse_p = '%s'", request.browse_p);
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
		} /* browse_p */
	
		/*
		** start_time condition
		*/
		if (request.start_time[0] != '\0')
		{
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}

			(void) sprintf (where, " (dp.end_time >= '%s' or dp.end_time is null)",
				request.start_time);
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

		} /* start_time */

		/*
		** end_time condition
		*/
		if (request.end_time[0] != '\0')
		{
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}

			(void) sprintf (where," (dp.start_time <= '%s' or dp.start_time is null)",
				request.end_time);
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);
	
		} /* end_time */
	
		/*
		** day_night condition
		*/
	
		if (request.day_night[0] != '\0')
		{
			if (where_init==1)
			{
				strcpy (where, AND);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				where_init = 1;
			}
		
			(void) sprintf (where, " (dp.day_night = '%s')", 
			                request.day_night);
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

		} /* day_night */

	}

	/*
	** set spatial_flag if any spatial specification was received
	*/
	if ( (request.region_type == POINT_LOC ) ||
		  (request.region_type == RANGE_LOC) ||
		  (request.region_type == POLYGON_LOC))
	{
		spatial_flag = 1;
	}

	/*
	** build spatial where clause if global_search is not true and spatial_flag
	** is set.
	*/
	if (spatial_flag == 1)
	{
		/*
		** latitude handling 
		**
		** The <where_clause> will look like:
		**
		**    ( dp.north_lat >= <request.minimum_latitude>
		**      and
		**      dp.south_lat <= <request.maximum_latitude>
		**    )
		*/

		strcpy (where, " and (");
		if (check_length
				(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);

		/*
		** latitude handling
		*/
		(void) sprintf (where, " dp.north_lat >= %-3.4f", request.minimum_latitude);
		if (check_length
				(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen (where);
			
		(void) sprintf (where, " and dp.south_lat <= %-3.4f)",
			request.maximum_latitude);
		if (check_length
				(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);				

		/*
		** longitude handling
		**	
		** There are two cases: the user query area either crosses 
		** 180/180 longitude or does not cross the meridian.   
		** Different <where clause>s are built based on the values of maximum 
		** longitude and minimum longitude.
		**
		** case 1: user specified area crosses 180/-180. Matching granules may:
		**
		**     1A: cross 180/-180
		**     1B: not cross 180/-180
		**
		** case 2: user specified area does not cross 180/-180 (including
		**         Point_Loc). Matching granules may:
		**
		**     2A: cross 180/-180
		**     2B: not cross 180/-180
		**
		** The <where_clause> for case 1A and 1B will be:
		**
		**   ( 
		**     ( dp.west_lon >= dp.east_lon                      <=== 1B
		**      	or 
		**       ( dp.west_lon <= <request.maximum_longitude>    <=== 1A
		**         or
		**         dp.east_lon >= <request.minimum_longitude>
		**       )
		**     ) 
		**   )
		**
		**
		** The <where_clause> for case 2A and 2B will be:
		**
		**   ( 
		**     ( dp.west_lon <= dp.east_lon                      <=== 2A
		**       and
		**       dp.west_lon <= <request.maximum_longitude>
		**       and
		**       dp.east_lon >= <request.minimum_longitude>
		**     )
		**     or
		**     ( dp.west_lon >= dp.east_lon                      <=== 2B
		**       and
		**       ( dp.west_lon <= <request.maximum_longitude>
		**         or
		**         dp.east_lon >= <request.minimum_longitude>
		**       )
		**     )
		**   )
		*/

	
		/*
		** 1A
		*/
		if (request.minimum_longitude > request.maximum_longitude)
		{
			strcpy (where, " and ((dp.west_lon >= dp.east_lon or (dp.west_lon <=");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

			(void) sprintf (where, " %-3.4f", request.maximum_longitude );
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

			(void) sprintf (where, " or dp.east_lon >= %-3.4f )))",
				request.minimum_longitude);
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

		}

		/*
		** 2A & 2B
		*/
		else
		{
			strcpy (where, " and ((dp.west_lon <= dp.east_lon and dp.west_lon <=");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

			(void) sprintf (where, " %-3.4f and dp.east_lon >= %-3.4f )",
				request.maximum_longitude, request.minimum_longitude);
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

			strcpy (where, " or (dp.west_lon >= dp.east_lon and ( dp.west_lon <=");
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

			(void) sprintf (where, " %-3.4f or dp.east_lon >= %-3.4f )))",
				request.maximum_longitude, request.minimum_longitude);
			if (check_length
					(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
			{
				return (IMS_ERROR);
			}
			where = where + strlen(where);

		}


	} /* end of spatial_flag == 1 */



	/*
	** sort the results by dataset_idx. This will save time later in granule
	** search.
	*/
	strcpy (where, " order by d.dataset_idx");
	if (check_length
			(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
	{
		return (IMS_ERROR);
	}
	where = where + strlen(where);

	(void) sprintf (v0Desc->query.sql, "select %s from %s where %s", v0Desc->query.select, 
		v0Desc->query.from, v0Desc->query.where); 

	return (IMS_OK);
} /* __build_ds */
 

/*************************************************************************
**
** v0_query__build_granule -
**
** purpose: creates the query string to retrieve matching granules for a 
**          dataset
**
** called by: v0_process__retrieve_granule_list
**
** return    IMS_OK  successful
**           IMS_FATAL system error
**           IMS_ERROR conflict occurred (temporal/seasonal specs do not
**                                        intersect)
**************************************************************************/

int v0_query__build_granule (V0_DESC_STRUCT *v0Desc)
{
	V0_DATASET_LIST   *curr_dataset;
	V0_REQUEST_STRUCT request;
	V0_KEYWORD_LIST   *temp_ptr;
	V0_VALUE_LIST     *t_ptr;
	char *select;
	char *from;
	char *where;
	char *sql;
	int  seasonal_flag, spatial_flag;
	int  s_doy, e_doy;     /* 1-366 */
	char start_doy[IMS_COL10_LEN+1], end_doy[IMS_COL10_LEN+1];         /*mm-dd*/
	char start_doy_mmdd[IMS_COL10_LEN+1], end_doy_mmdd[IMS_COL10_LEN+1];/*mmdd*/
	int  i, i_count;
	int  ignore_first_interval, ignore_last_interval;
	int  first_year,last_year;
	char order_by[IMS_COL20_LEN+1];
	int  list_init;
	int  where_count;

	curr_dataset = v0Desc->result.curr_dataset;
	request      = v0Desc->request;

	/*
	** Initialization.
	*/
	select = v0Desc->query.select;
	from = v0Desc->query.from;
	where = v0Desc->query.where;
	sql = v0Desc->query.sql;
	seasonal_flag = 0;   /* set to 1 if seasonal search is specified */
	spatial_flag  = 0;   /* set to 1 if spatial search is specified */
	s_doy = e_doy = 0;   /* 1 - 366 */
	start_doy[0]='\0';   /* mm-dd */
	start_doy_mmdd[0]='\0'; /* mmdd */
	end_doy[0]='\0';
	end_doy_mmdd[0]='\0';
	first_year = last_year = 0;
	order_by[0]='\0';
	where_count= 0;

	/*
	** common <select clause>
	*/
	strcpy (select, " granule_idx, name");
	select = select + strlen(select);

	/*
	** Evaluate whether any seasonal search is needed.  Seasonal search is based
	** on a range of time represented by day_of_year, and repeated every year
	** within the range of temporal start/end times.
	** 
	** Seasonal search query will not be built if any of the following
	** condition occurs:
	**
	** 1. No day_of_year specified as a search criteria.  Only temproal search
	**    will be performed in this case.
	**
	** 2. All day_of_year intervals fall outside the temporal range.  A 
	**    IMS_ERROR will be returned, and no query will be built.  
	**    
	*/

	/*
	** set spatial_flag if the spatial search are verified 
	*/
	if ((request.region_type == POINT_LOC) || 
	    (request.region_type == RANGE_LOC) || 
	    (request.region_type == POLYGON_LOC) || 
	    (request.global_granules_p == 'Y')) 
	{
		spatial_flag = 1;
	}

	if ( (request.start_day_of_year != 0) ||
		  (request.stop_day_of_year != 0) )
	{
		if (request.start_day_of_year == 0)
		{
			s_doy = 1;	
			e_doy = request.stop_day_of_year;
		}
		else if (request.stop_day_of_year == 0)
		{
			s_doy = request.start_day_of_year;
			e_doy = 366;   /* This is a default value, the conversion code will
									take care of the non-leap years */
		}
		else
		{
			s_doy = request.start_day_of_year;
			e_doy = request.stop_day_of_year;
		}

		seasonal_flag = 1;
	}
	
	if (seasonal_flag)
	{
		i_count = 0;
		ignore_first_interval = ignore_last_interval = 0;
		
		
		/*
		** convert doy to mm-dd and mmdd format.  The mmdd will be utilized 
		** to position the seasonal time interval to check against the termporal
		** time range for intersection.
		*/
		
		(void) v0_time__DoyToDate (s_doy, atoi(request.start_time_year),
						start_doy, start_doy_mmdd);   
						  
		(void) v0_time__DoyToDate (e_doy, atoi(request.end_time_year),
						end_doy, end_doy_mmdd);   
						  

		/*
		** a simple loop is needed here to go thru the years covered by the 
		** temporal time range and check if there's any intersection between 
		** temporal and seasonal
		** time range.
		**
		** e.g. seasonal search on doy 20-30 does not intersect with temporal
		** search 06-01-90 to 01-10-91
		*/

		first_year = atoi (request.start_time_year);
		last_year = atoi (request.end_time_year);
			
		for (i  = first_year;
			  i <= last_year;
			  i++)
		{
			if (i == first_year)
			{
			
				/* 
				** if this is the first year, check if the seasonal interval 
				** intersects with the temporal time range
				*/

				if ( atoi(end_doy_mmdd) < atoi(request.start_time_mmdd ) )
				{
					ignore_first_interval = 1;
				}
				else
				{
					i_count++;
				}

			}		
			else if (i == last_year)
			{	
				/* 
				** if this is the last year, check if the seasonal interval 
				** intersects with the temporal time range.  
				*/
	
				if (atoi(start_doy_mmdd) > atoi(request.end_time_mmdd))
				{
					ignore_last_interval = 1;
				}
				else
				{
					i_count++;
				}
			}	
			else
			{
				i_count ++;
			}	
		}  /* for */

		/*
		** if the seasonal interval does not intersect with the temporal range
		** return with IMS_ERROR, which results a 'no match found' status in 
		** the calling routine
		*/
		if (i_count == 0)
		{
			return (IMS_ERROR);
		}	

		/*
		** redefine the starting/ending year for repeating seasonal granule
		** retrieval, if needed.
		*/
		if (ignore_first_interval)
		{
			first_year += 1;
		}

		if (ignore_last_interval)
		{
			last_year -= 1;
		}

	}  /* if seasonal flag is true */


	/*
	** begin constructing the query statement for granules
	**
	**  The format is:
	**
	**    select <select_list> from <granules_table> where <where_clause >
	*/

	/*
	** a common condition for all granule searches
	*/
	(void )sprintf (where, " where ims_visible_p = 'Y' and status = %d", 
 	                  IMS_AVAILABLE);
	if (check_length
			(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
	{
		return (IMS_ERROR);
	}
	where = where + strlen(where);

	/*
	** granule id search     
	*/
	if (request.granule_id != (V0_VALUE_LIST *)NULL)
	{
		t_ptr = request.granule_id;
		list_init = 0;

		(void )strcpy (where, " and ("); 
		if (check_length
			(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);

		while (t_ptr != (V0_VALUE_LIST *)NULL)
		{

			if (list_init == 0)
			{
				list_init = 1;
			}
			else if (list_init == 1)
			{
				strcpy (where, " or");
				where = where + strlen(where);
			}

			(void) sprintf (where, " name like \"%s\"", t_ptr->char_value1);
			where = where + strlen (where);

			t_ptr = t_ptr->next_p;
		}

		(void )strcpy (where, ")"); 
		if (check_length
			(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);
	}

	/*
	** end of granule id search
	*/

	if (curr_dataset->temporal_type != 0)
	{
		switch (curr_dataset->temporal_type)
		{
			case 1:
				/*
				** temporal_type 1 is center time or similar
				**
				** The query may look as:
				**      
				**   select granule_idx, name, <center_time>, <center_time>,
				**   ,<spatial_keyword_list> 
				**   from <granules_table> where <where_clause>
				**
				**
				** The <where clause> are in different format depends on the type of 
				** search
				**
				** 1) temporal search only (i.e. seasonal_flag is false):
				**
				**    where ( start_time <= '<user_spec_end_time>' and
				**            end_time >= '<user_spec_start_time>' )
				**
				** 2) temporal+seasonal search (i.e. seasonal_flag is true) requires
				**    a concatination of the above query and the following:
				**
				**    and ( (start_time <= '<mm-dd of end doy>-<year>' and
				**             end_time >= '<mm-dd of start doy>-<year>' )
				**           or ....
				**          )
				**                        
				**    the "start_time ... <year>" query segment will be iterated
				**    for each year from 'first_year' to 'last_year' 
				**
				*/
	
				/* Build the select */
				(void) sprintf (select, ", %s, %s",
					curr_dataset->temporal_key_list->keyword,
					curr_dataset->temporal_key_list->keyword);
				select = select + strlen(select);
	
				/* 
				** Build the common where clause for both temporal and seasonal 
				** search 
				*/
				if ( (request.start_time[0] != '\0' ) &&
				  	(request.end_time[0] != '\0' ))
				{
	
					(void) sprintf (where, " and (start_time <= '%s') and (end_time >= '%s')",
						request.end_time,
						request.start_time);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + (strlen(where));
				}
	
				if (seasonal_flag)	
				{
					/* Build the where clause for temporal+seasonal search  */
	
					strcpy (where, " and (");
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
	
					for (i=first_year; i <= last_year; i++)
					{
						if ( i != first_year)
						{
							strcpy (where, " or");
							if (check_length
									(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
							{
								return (IMS_ERROR);
							}
							where = where + strlen (where);
						}
	
			
						/* 
						** the mm-dd conversion is needed for each loop because
						** of possible leap year
						*/
						(void) v0_time__DoyToDate (s_doy, i, start_doy, start_doy_mmdd); 
						(void) v0_time__DoyToDate (e_doy, i, end_doy, end_doy_mmdd); 
						 	 
	
						(void) sprintf ( where,
							" (start_time <= '%s-%d' and end_time >= '%s-%d')",
							end_doy, i, start_doy, i);
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen (where);
					}
	
					strcpy (where, ")");
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen (where);
	
				} /* if seasonal_flag */
	
				break;

			case 2:

				/*
				** temporal_type 2 is start/end time or similar
				**
				** The query may look as:
				**      
				**   select granule_idx, name, <start_time>, <end_time>,
				**   , <spatial_keyword_list> 
				**   from <granules_table> where <where_clause> 
				**
				**
				** The <where clause> are in different format depends on the type of 
				** search
				**
				** 1) temporal search only (i.e. seasonal_flag is false):
				**
				**    where ( start_time <= '<user_spec_end_time>' and
				**            end_time   >= '<user_spec_start_time>' )
				**
				**    which retrieves granules that meet any of the following
				**    conditions:
				**
				**     user_spec_start_time                    user_spec_end_time
				**        |                                              |
				** ================================================================== 
				**                      |-- granule 1 --|
				**        |---------------- granule 2 -------------------|
				**  |---------------------- granule 3 ----------------------|
				** |- granule 4 -|
				**                                                   |- granule 5 -|
				**
				**
				** 2) temporal+seasonal search (i.e. seasonal_flag is true) requires
				**    a concatination of the above query and the following:
				**
				**    and ( (start_time <= '<mm-dd of end doy>-<year>' and
				**           end_time   >= '<mm-dd of start doy>-<year>' )
				**           or ....
				**         )
				**                        
				**    the "start_time ... <year>" query segment will be repeated
				**    for each year from 'first_year' to 'last_year' 
				**
				*/
	
				/* Build the select */
				(void) sprintf (select, ", %s, %s",
					curr_dataset->temporal_key_list->keyword,
					curr_dataset->temporal_key_list->next_p->keyword);
				select = select + strlen(select);		
	
				/* Build the common where clause for both type of  search */
				if ( (request.start_time[0] != '\0' ) &&
				  	(request.end_time[0] != '\0'))
				{
	
					(void) sprintf (where, " and (start_time <= '%s') and (end_time>= '%s')",
						request.end_time,
						request.start_time);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + (strlen(where));
				}
	
				if (seasonal_flag)
				{
					/* Build the where clause for temporal+seasonal search  */
	
					strcpy (where, " and (");
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
	
					for (i=first_year; i <= last_year; i++)
					{
						if ( i != first_year)
						{
							strcpy (where, " or");
							if (check_length
									(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
							{
								return (IMS_ERROR);
							}
							where = where + strlen (where);
						}
			
						/* 
						** the mm-dd conversion is needed for each loop because
						** of possible leap year
						*/
						(void) v0_time__DoyToDate (s_doy, i, start_doy, start_doy_mmdd); 
						(void) v0_time__DoyToDate (e_doy, i, end_doy, end_doy_mmdd); 
						 	 
	
						(void) sprintf ( where,
							" (start_time <= '%s-%d' and end_time >= '%s-%d')",
							end_doy, i, start_doy, i);
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen (where);
					} 
	
					strcpy (where, ")");
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen (where);
	
				}  /* if seasonal_glag */
	
			break;
	
			default:
				return (IMS_FATAL);
	
		} /* switch temporal_type */
	
		/* make sure all the keyword is not null */
		strcpy (where, " and (");
		if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);
	
		temp_ptr = curr_dataset->temporal_key_list;
	
		for (i_count=0; i_count < curr_dataset->temporal_key_count; i_count++)
		{
			if (i_count == 0)
			{
				(void) sprintf (where,"%s is not null", temp_ptr->keyword);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				(void) sprintf (where," and %s is not null", temp_ptr->keyword);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
		
			temp_ptr = temp_ptr->next_p;
		}
	
		strcpy (where, ")");
		if (check_length(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);

	} /* if temporal_type != 0 */


	/*
	** build spatial where clause if global_search is not true and spatial_flag
	** is set.
	*/
	if (curr_dataset->spatial_type != 0)
	{
		if (spatial_flag == 1)
		{
			/*
			** build the select clause to select on the fields accroding
			** to the spatial_type
			*/
	
			temp_ptr = curr_dataset->spatial_key_list;
	
			for (i_count=0; i_count < curr_dataset->spatial_key_count; i_count++)
			{
				(void) sprintf (select,", %s", temp_ptr->keyword);
				select = select + strlen(select);
	
				temp_ptr = temp_ptr->next_p;
			}
	
			if (curr_dataset->spatial_type == 4)
			{
				strcpy (select, ", pole_included, north_lat, south_lat");
			select = select + strlen(select);
			}
		
			if ((request.global_search == 'N') && 
			 	(request.global_granules_p == 'N'))
			{
	
				/*
				** let's handle special cases first 
				*/
				if ((request.north_pole_only == 1) || (request.south_pole_only == 1)) 
				{
					strcpy (where, "  and (");
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
	
					if (request.north_pole_only == 1)  
					{
						strcpy (where, " pole_included = 'N' or north_lat = 90.0 )");
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
					}
					else if (request.south_pole_only == 1) 
					{
						strcpy (where, " pole_included= 'S' or south_lat = -90.0 )");
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
					}
	
				} /* end of special case handling */
	

				else
				{
					/*
					** latitude handling 
					**
					** The <where_clause> will look like:
					**
					**    ( north_lat >= <request.minimum_latitude>
					**      and
					**      south_lat <= <request.maximum_latitude>
					**    )
					*/
					strcpy (where, " and (");
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);
			
					/*
					** latitude handling
					*/
					(void) sprintf (where, " north_lat >= %-3.4f", request.minimum_latitude);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen (where);
						
					(void) sprintf (where, " and south_lat <= %-3.4f)",
						request.maximum_latitude);
					if (check_length
							(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
					{
						return (IMS_ERROR);
					}
					where = where + strlen(where);				
	
					/*
					** longitude handling
					**	
					** Two cases: the user query area either crosses 180/-180 longitude 
					** or does not cross the meridian.  
					** Different <where clause>s are built based on the values of 
					** maximum longitude and minimum longitude.
					**
					** case 1: user specified area crosses 180/-180. 
					** Matching granules may:
					**
					**     1A: cross 180/-180
					**     1B: not cross 180/-180
					**
					** case 2: user specified area does not cross 180/-180 (including
					**         Point_Loc). Matching granules may:
					**
					**     2A: cross 180/-180
					**     2B: not cross 180/-180
					**
					** The <where_clause> for case 1A and 1B will be:
					**
					**   ( 
					**     ( west_lon >= east_lon                      <=== 1B
					**      	or 
					**       ( west_lon <= <request.maximum_longitude> <=== 1A
					**         or
					**         east_lon >= <request.minimum_longitude>
					**       )
					**     ) 
					**   )
					**
					**
					** The <where_clause> for case 2A and 2B will be:
					**
					**   ( 
					**     ( west_lon <= east_lon                      <=== 2A
					**       and
					**       west_lon <= <request.maximum_longitude>
					**       and
					**       east_lon >= <request.minimum_longitude>
					**     )
					**     or
					**     ( west_lon >= east_lon                      <=== 2B
					**       and
					**       ( west_lon <= <request.maximum_longitude>
					**         or
					**         east_lon >= <request.minimum_longitude>
					**       )
					**     )
					**   )
					*/
		
		
					/*
					** 1A
					*/
					if (request.minimum_longitude > request.maximum_longitude)
					{
						strcpy (where, " and (( west_lon >= east_lon or ( west_lon <=");
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
			
						(void) sprintf (where, " %-3.4f", request.maximum_longitude );
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
			
						(void) sprintf (where, " or east_lon >= %-3.4f )))",
							request.minimum_longitude);
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
			
					}
			
					/*
					** 2A & 2B
					*/
					else
					{
					
						strcpy (where, " and ( ( west_lon <= east_lon and west_lon <=");
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
			
						(void) sprintf (where, " %-3.4f and east_lon >= %-3.4f )",
							request.maximum_longitude, request.minimum_longitude);
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
			
						strcpy (where, " or ( west_lon >= east_lon and ( west_lon <=");
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
			
						(void) sprintf (where, " %-3.4f or east_lon >= %-3.4f )))",
						request.maximum_longitude, request.minimum_longitude);
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);
			
					}
	
					/*
					** to include granules on the other hemisphere which interset with
					** the user area on one point only, i.e. the pole
					*/
					if (request.maximum_latitude == 90.0)
					{
						strcpy ( where, " or north_lat = 90.0");
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);;
					}
	
					if (request.minimum_latitude == -90.0 )
					{
						strcpy ( where, " or south_lat = -90.0");
						if (check_length
								(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
						{
							return (IMS_ERROR);
						}
						where = where + strlen(where);;
					}
	
				}/* if not a polar point region */
			
			} /* end of building spatial query for non-global searches */

		} /* end of spatial_flag == 1 */

	
		/* make sure all the keywords exist */
		strcpy (where, " and (");
		if (check_length
				(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);
	
		temp_ptr = curr_dataset->spatial_key_list;
	
		for (i_count=0; i_count < curr_dataset->spatial_key_count; i_count++)
		{
			if (i_count == 0)
			{
				(void) sprintf (where,"%s is not null", temp_ptr->keyword);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
			else
			{
				(void) sprintf (where," and %s is not null", temp_ptr->keyword);
				if (check_length
						(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
				{
					return (IMS_ERROR);
				}
				where = where + strlen(where);
			}
		
			temp_ptr = temp_ptr->next_p;
		}
	
		strcpy (where, ")");
		if (check_length
				(v0Desc,&where_count,strlen(where),V0_MAX_WHERE_LEN)<IMS_OK) 
		{
			return (IMS_ERROR);
		}
		where = where + strlen(where);

	} /* if spatial type is not 0 */

	/*
	** detail keyword <select clause>
	*/
	temp_ptr = curr_dataset->detail_key_list;

	while (temp_ptr != (V0_KEYWORD_LIST *)NULL)
	{
		sprintf (select, ", %s", temp_ptr->keyword);
		select = select + strlen(select);

		temp_ptr = temp_ptr->next_p;
	}

	/*
	** V0 expects returned granules are sorted by time. The sorting will
	** be done in the caller function.
	*/

	(void) sprintf (v0Desc->query.sql, "select %s from %s %s", 
		v0Desc->query.select, curr_dataset->granules_table, v0Desc->query.where );

	return (IMS_OK);
} /* v0_query__build_granule */

/*
************************************************************************
**
** check_length - calculates total lenght of a text field and check if
**                the total length exceeds the maximum. 
**                Updates the odl_status if needed.
**
** return       - IMS_OK if the length is okay  
**              - IMS_ERROR if the total length exceeds maximum 
**
************************************************************************
*/
static int check_length (V0_DESC_STRUCT *v0Desc,
												 int *new_length,
												 int str_length, 
                         int max_length)
{
	int wc;

	wc = *new_length;
	wc = wc + str_length;

	if (wc >= max_length)
	{
		if (v0Desc != (V0_DESC_STRUCT *)NULL)
		{
			if ( (v0Desc->RxType == V0_DIRECTORY_SEARCH) ||
					 (v0Desc->RxType == V0_INVENTORY_SEARCH))
			{
				strcpy (v0Desc->odl_status,"12");
				ims_msg (v0Desc->msgDesc, IMS_ERROR,
				"v0_query__check_length: query string exceeded database "
				"allowable length.");
			}
		}

		return (IMS_ERROR);
	}

	*new_length = wc;
	return (IMS_OK);

} /* end of check_length */

