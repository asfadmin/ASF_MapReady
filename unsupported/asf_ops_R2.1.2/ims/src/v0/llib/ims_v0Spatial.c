static char *sccs = "@(#)ims_v0Spatial.c	5.4  12/19/96";
/************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0Spatial.c
**
** Purpose
**		IMS/DADS spatial search related routines.
**
**	Creator   :   Julie Wang
**
**	Date      :   December 13, 1994
**
** Modifications:
**
**   02/20/96    jwang   R1Bprime (preliminary).
**
**   01/22/96    jwang   Added conversion function from rectangle to 
**                       four points
**
**   06/05/95    jwang   Changed all reference of cdb_ or CDB_ to 
**                       ims_ or IMS_
**
**   03/31/95    jwang   Spatial search pass 2 
**
**   02/01/95    jwang   Spatial search pass 1
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

static float longitude_conversion (float, int);

/**************************************************************************
**
** v0_calcb - interface routine for the external callers.  
**
** input:  coordinates of four corner points of a region. 
**         Note:
**          - the points have to be in CLOCKWISE order starting from NW.
**          - all latitudes are in the range of -90.0 to 90.0
**          - all longitudes are in the range of -180 to 180.0
**          - this routine CAN NOT handler areas which cover larger than
**            half a globe 
**
** output: min/max lat/lon of the area, and pole_included info 
**
** return:  IMS_OK if successful
**              -1 if invalid latitude value provided
**              -2 if invalid longitude value provided
**
***************************************************************************/
int v0_spatial__v0_calcb 
	(float *lat, float *lon, float *minLat, float *maxLat, 
	 float *minLon, float *maxLon, char *pole_included)

{
	V0_REQUEST_STRUCT temp_req;
	int     i, status;

	/*
	** initialization
	*/
	temp_req.minimum_latitude  = 0.0;
	temp_req.maximum_latitude  = 0.0;
	temp_req.minimum_longitude = 0.0;
	temp_req.maximum_longitude = 0.0;
	temp_req.pole_included     = ' ';;
	temp_req.region_type = POLYGON_LOC;
	temp_req.map_projection_type = NO_PROJECTION;

	for (i=0; i<PT_MAX; i++)
	{
		if ( (lat[i] > 90.0) || (lat[i] < -90.0))
		{
			return (-1);
		}
		else
		{
			temp_req.u_lat[i] = lat[i];
		}

		if ( (lon[i] > 180.0) || (lon[i] < -180.0))
		{
			return (-2);
		}	
		else
		{
			temp_req.u_lon[i] = lon[i];
		}
	}

	(void) v0_spatial__v0_boundaries (&temp_req); 

	*minLat = temp_req.minimum_latitude;
	*maxLat = temp_req.maximum_latitude;
	*minLon = temp_req.minimum_longitude;
	*maxLon = temp_req.maximum_longitude;

	*pole_included = temp_req.pole_included;

	return (IMS_OK);
} /* v0_calcb */


/************************************************************************
**
** v0_boundaries -- calculate min/max lat/lon and other spatial search 
**     factors 
**
**
** LIMITATION in handling polygon_loc (four corners):
**             1. this routine assumes 4 pts are in clockwise order.  
**             2. regions crossing North or South pole may not have minimum
**                latitude on the other hemisphere, i.e. only one hemispere
**                will be handled when the input region crosses a pole
**             3. region marking cannot cross domain, e.g. a region covers
**                Asia(positive lon), America(negative lon), and 
**                Africa(positivelon) is not recommended.   
**
** return:	IMS_OK     if successful 
**          -1         if wrong info was passed from the client
**          -2         if errorneous coordinate value were received 
**
*************************************************************************/
int v0_spatial__v0_boundaries (V0_REQUEST_STRUCT *request)
{
	
	REGION np_region, sp_region, polygon_region, line_region;
	REGION above_equator_region;
	int    i;
	float  max_lat, min_lat, max_lon, min_lon;
	int    cross_180;         /* a flag of whether a polygon area crosses */
									  /* longitude -180/180 */										
	float  temp_lon;
	float  temp_lat;

	/*
	** initialization
	*/
	cross_180= 0;

	/*
	** region initialization for north pole
	*/
	np_region.geographic_type = POINT;
	np_region.pole_included     = 'N';
	np_region.projection        = POLAR_STEREOGRAPHIC;
	np_region.latitude[0]       = 90.0;
	np_region.longitude[0]      = 0.0;
	np_region.tangent_latitude  = 0.0;
	np_region.tangent_longitude = 0.0;

	/*
	** region initialization for south pole
	*/
	sp_region.geographic_type = POINT;
	sp_region.pole_included     = 'S';
	sp_region.projection        = POLAR_STEREOGRAPHIC;
	sp_region.latitude[0]       = -90.0;
	sp_region.longitude[0]      = 0.0;
	sp_region.tangent_latitude  = 0.0;
	sp_region.tangent_longitude = 0.0;

	/*
	** polygon_region initialization
	*/
	polygon_region.geographic_type = POLYGON;
	polygon_region.tangent_latitude  = request->tangent_latitude;
	polygon_region.tangent_longitude = request->tangent_longitude;

	switch (request->region_type)
	{
		case POINT_LOC:

			request->maximum_latitude = request->u_lat[0];
			request->minimum_latitude = request->u_lat[0];
			request->maximum_longitude= request->u_lon[0];
			request->minimum_longitude= request->u_lon[0];

			if ( request->u_lat[0] == 90.0)
			{
				request->north_pole_only = 1;
			}
			else if (request->u_lat[0] == -90.0)
			{
				request->south_pole_only = 1;
			}

			break;


		case RANGE_LOC:

			/*
			** It is assumed here that v0 users are not allowed to mark/specify 
			** any rectangular region crossing the North/South pole
			** If v0 client takes away this constrain, this code will have to be
			** revisited.   -jlw-
			*/

			request->minimum_latitude = request->u_min_lat;
			request->maximum_latitude = request->u_max_lat;
			request->minimum_longitude= request->u_min_lon;
			request->maximum_longitude= request->u_max_lon;

			if ((request->u_max_lat == 90.0) && (request->u_min_lat == -90.0))
			{
				request->pole_included = 'B';
			}
			else if ( (request->u_min_lat == 90.0) && (request->u_max_lat == 90.0) )
			{
				request->north_pole_only = 1;
			}
			else if ( (request->u_min_lat == -90.0) && (request->u_max_lat == -90.0) )
			{
				request->south_pole_only = 1;
			}
			break;



		case POLYGON_LOC:
			/*
			** prepare region data for intersection analysis
			*/
			for (i=0; i<PT_MAX; i++)
			{
				polygon_region.latitude[i] = request->u_lat[i];
				if ( (polygon_region.latitude[i] > 90.0) ||
					  (polygon_region.latitude[i] < -90.0))
				{
					return (-2);
				}
				polygon_region.longitude[i] = request->u_lon[i];
				if ( (polygon_region.longitude[i] > 180.0) ||
					  (polygon_region.longitude[i] < -180.0))
				{
					return (-2);
				}
	
				
			}

		
			/*
			** calculate min/max latitudes for any type of projection
			*/
			max_lat = -90.0;
			min_lat =  90.0;
	
			for (i=0; i<PT_MAX; i++)
			{
				if (polygon_region.latitude[i] > max_lat)
				{
					max_lat = polygon_region.latitude[i];
				}
	
				if (polygon_region.latitude[i] < min_lat)
				{
					min_lat = polygon_region.latitude[i];
				}
			}
		
			/* 
			** examine special cases first
			*/
			if ( (max_lat == 90.0) && (min_lat == 90.0) )	
			{
				request->pole_included = 'N';
				request->north_pole_only = 1;
				request->minimum_latitude = 90.0;
				request->maximum_latitude = 90.0;
				request->minimum_longitude = 0.0;
				request->maximum_longitude = 0.0;
#ifdef SDEBUG
					printf ("\nuser region covers ONLY North pole." );
#endif
				return (IMS_OK);
			}

			else if ( (max_lat == -90.0) && (min_lat == -90.0) )
			{
				request->pole_included = 'S';
				request->south_pole_only = 1;
				request->minimum_latitude = -90.0;
				request->maximum_latitude = -90.0;
				request->minimum_longitude = 0.0;
				request->maximum_longitude = 0.0;
#ifdef SDEBUG
					printf ("\nuser region covers ONLY South pole." );
#endif
				return (IMS_OK);
			}

			else if ( (max_lat == 90.0) && (min_lat == -90))  
			{
							
					request->pole_included = 'B';
#ifdef SDEBUG
					printf ("\nuser region touches both South and North poles." );
#endif
			}

			else if (min_lat == -90.0)
			{
							
#ifdef SDEBUG
				printf ("\nuser region touches South pole.");
#endif

				request->pole_included = 'S';
			}

			else if (max_lat == 90.0)
			{
				request->pole_included = 'N';
#ifdef SDEBUG
				printf ("\nuser region touches Nouth pole.");
#endif
			}

			/*
			**  Verify whether and which pole is included in the region.
			**
			**  The assumption is that the user can graphically select a search area 
			**  covering the north pole only when using NORTH_POLAR_
			**  STEREOGRAPHIC map, or SOUTH_POLAR_STEREOGRAPHIC map. 
			**
			**  
			*/
			if ( (min_lat>= 0.0) && (max_lat != 90.0))  
			{
	
				polygon_region.projection        = POLAR_STEREOGRAPHIC;

				if ( (i=spatial_query_routines__point_in_polygon
							(np_region, polygon_region)) == 1) 
				{
					/*
					** if the area covers north or south pole,the max/min lat will
					** be the same as the lat of north or south pole.  And maximum
					** minimum lon will be 180 and -180.  Which means, a spacial
					** 'box' is defined as the boundaries for the area
					**
					*/

					/*
					** if max_lat = 90.0, i.e. touch the pole by one point, the area will be 
					** checked later
					*/
					request->pole_included = 'N';

					request->minimum_latitude = min_lat;
					request->maximum_latitude = 90.0;
					request->minimum_longitude = -180.0;
					request->maximum_longitude  = 180.0;
#					ifdef SDEBUG
						printf("\nRegion crosses North pole with minimum latitude %3.4f\n",
						min_lat);
#					endif

				} /* if contains North pole */
	
			}
	
			else if  ( (max_lat <= 0.0) && (min_lat != -90.0))
			{
	
				polygon_region.projection        = POLAR_STEREOGRAPHIC;

				if ( (i=spatial_query_routines__point_in_polygon
							(sp_region, polygon_region))== 1) 
				{
					request->pole_included = 'S';
	
					request->minimum_latitude = -90.0;
					request->maximum_latitude = max_lat;
					request->minimum_longitude = -180.0;
					request->maximum_longitude  = 180.0;
#					ifdef SDEBUG
						printf("\nRegion crosses South pole with maximum latitude %3.4f\n",
							max_lat);
#					endif

				}
	
			}
	

			/*
			** all non pole crossing region or region intersect with pole with one point 
			** will be 'boxed' by their min/max lat/lon.  
			**
			** we will examine all polar_stererographic non pole-crossing 
			** regions, and plate_carree projections (i.e. user marked on 
			** WORLD map or type in four corner values)
			*/
			
			if ( (request->pole_included == ' ') ||
				  (min_lat == -90.0) ||
				  (max_lat ==  90.0))
			{

				cross_180 = 0;

				/*
				** if the region is entirely on the north hemisphere
				*/
				if (min_lat >= 0.00)
				{

					polygon_region.projection        = POLAR_STEREOGRAPHIC; 
					polygon_region.tangent_latitude  = request->tangent_latitude;
					polygon_region.tangent_longitude = request->tangent_longitude;
	
	
					/*
					** check if a line with 180 meridian is fully enclosed within the
					** user region.  If yes, we identify the area as crossing 180.
					*/
					if (max_lat == 90.0)
					{
						/*
						** special case, if a user area touches the pole, than we have to 
						** make sure the line is not crossin pole, otherwise the line may
						** intersect the user area on the pole only which is not an
						** actual 180 crossing.
						*/
						if (min_lat >= 89.0)
						{
							line_region.latitude[0] = line_region.latitude[1] = 
								(90.0+min_lat)/2.0;
						}
						else
						{
							line_region.latitude[0] = line_region.latitude[1]= 89.0;
						}
					}
					else
					{
						line_region.latitude[0] = line_region.latitude[1] = max_lat;
					}

					line_region.latitude[2] = line_region.latitude[3]= 0.0;

					line_region.longitude[0] = line_region.longitude[3]= 180.00;
					line_region.longitude[1] = line_region.longitude[2]= -180.00;
					line_region.projection  = POLAR_STEREOGRAPHIC;
					line_region.geographic_type = POLYGON;

	
					/*
					** does a region on north hemisphere overlay with lon 180/-180?
					*/
					if ( (i = spatial_query_routines__do_regions_overlap 
						(polygon_region, line_region)) == 1 ) 
					{
						cross_180 =1;
					}
	

#					ifdef SDEBUG
						if (cross_180==1)
							printf ("\nuser area overlay with 180 lon");
						else if (i<0)
							printf("\n180 lon overlay check got an error of %d\n",i);
#					endif
	
	
				}


				else if (max_lat <= 0.0)
				{

					polygon_region.projection        = POLAR_STEREOGRAPHIC; 
					polygon_region.tangent_latitude  = request->tangent_latitude;
					polygon_region.tangent_longitude = request->tangent_longitude;
	
					line_region.latitude[0] = line_region.latitude[1] = 0.0;
	
					if (min_lat == -90.0)
					{
						if (max_lat <= -89.0)
						{
							line_region.latitude[2] = line_region.latitude[3]= (max_lat-90.0)/2.0;
						}
						else
						{
							line_region.latitude[2] = line_region.latitude[3]= -89.0;
						}
					}
					else
					{
						line_region.latitude[2] = line_region.latitude[3] = min_lat;
					}

					line_region.longitude[0]= line_region.longitude[3]=180.00;
					line_region.longitude[1]= line_region.longitude[2]=-180.00;
					line_region.projection  = POLAR_STEREOGRAPHIC;
					line_region.geographic_type = POLYGON;


					/*
					** overlay with lon -180/180?
					*/
					if ( (i = spatial_query_routines__do_regions_overlap 
							(polygon_region, line_region)) ==1) 
					{
						cross_180 =1;
					}
	
#					ifdef SDEBUG
						if (cross_180==1)
							printf ("\nuser area overlay with 180 lon");
						else if (i<0)
							printf("\n180 lon overlay check got an error of %d\n",i);
#					endif


				}


				/*
				** The region crosses equator.
				**
				** A temporary region (i.e. above_equator_region) is generated to 
				** have the same longitudes with the user region and 
				** max_lat as latitudes of first two points and min_lat as 
				** latitudes of the last two point.  The assumption is, if  
				** above_equator_region overlays with 180/-180, the user region 
				** should have the same porperty.  It is done this way in order 
				** to utilize the NSIDC spatial_query_routines with 
				** POLAR STEREOGRAPHICAL projection type.
				*/
				else	
				{
		
					DEFAULT_REGION (above_equator_region);

					for(i=0; i<PT_MAX; i++) 
					{
						above_equator_region.longitude[i]
							= polygon_region.longitude[i];
					}
	
					if (max_lat == 90.0)
					{
						above_equator_region.latitude[0]=  89.0; 
						above_equator_region.latitude[1]=  89.0; 
					}
					else
					{
						above_equator_region.latitude[0]=  max_lat; 
						above_equator_region.latitude[1]=  max_lat; 
					}
					above_equator_region.latitude[2] = 0.0;
					above_equator_region.latitude[3] = 0.0;
					above_equator_region.geographic_type = POLYGON;
					above_equator_region.projection = POLAR_STEREOGRAPHIC;
	
					line_region.latitude[0] = line_region.latitude[1]= 89.0;
					line_region.latitude[2] = line_region.latitude[3]=0.00;
					line_region.longitude[0] = line_region.longitude[3] = 180.00;
					line_region.longitude[1] = line_region.longitude[2] = -180.00;
					line_region.projection  = POLAR_STEREOGRAPHIC;
					line_region.geographic_type = POLYGON;
	
					/*
					** does a region on north hemisphere overlay with lon -180/180?
					*/
					if ( (i = spatial_query_routines__do_regions_overlap 
						(above_equator_region, line_region)) == 1 ) 
					{
						cross_180 =1;
					}
	
#					ifdef SDEBUG
						if (cross_180==1)
							printf ("\nuser area overlay with 180 lon");
						else if (i <0)
							printf("\n180 lon check got an error of %d\n",i);
#					endif
	
				}		


				/*
				** The calculation of min/max longitudes for polygon region crosses
				** or not-crosses 0 is different.  
				**
				** if the region crosses 180/-180, we'll do a conversion of lon
				** first (to become 0->360).  The algorithm used here cannot 
				** handle the calculation of min/max with both max_lon and min_lon 
				** be 0.0 Once the min/max is found. The lon will be converted
				** back when the calculation is done.
				** 
				*/
	
				if (cross_180)
				{
					request->cross_180 = 1;

					max_lon = 0.0;
					min_lon = 360.0;
	
					for (i=0; i<PT_MAX; i++)
					{
						temp_lon= longitude_conversion(polygon_region.longitude[i],1);
	
						if (temp_lon > max_lon)
						{
							max_lon = temp_lon;
						}
	
						if (temp_lon < min_lon)
						{
							min_lon = temp_lon;
						}
					}
	
					min_lon = longitude_conversion (min_lon,2);
					max_lon = longitude_conversion (max_lon,2);
					
				}
	
				else 
				{
					max_lon = -180.0;
					min_lon =  180.0;
	
					for (i=0; i<PT_MAX; i++)
					{
						if (polygon_region.longitude[i] > max_lon)
						{
							max_lon = polygon_region.longitude[i];
						}
	
						if (polygon_region.longitude[i] < min_lon)
						{
							min_lon = polygon_region.longitude[i];
						}
					}
	
				}
				
	
				request->minimum_latitude = min_lat;
				request->maximum_latitude = max_lat;
				request->minimum_longitude = min_lon;
				request->maximum_longitude = max_lon;
	
			} /* end if non pole crossing */
	


			if ( ( (request->minimum_latitude == -90.0) &&
				    (request->maximum_latitude == 90.0)  &&
				    (request->minimum_longitude== 180.0) &&
				    (request->maximum_longitude== -180.0) ) ||
			     ( (request->minimum_latitude == -90.0) &&
				    (request->maximum_latitude == 90.0)  &&
				    (request->minimum_longitude== -180.0) &&
				    (request->maximum_longitude== 180.0) ) )
	
			{
					request->global_search = 'Y';
			}



		break;	

		default:
			
			return (-1);

	} /* end of switch (request->region_type) */

#		ifdef SDEBUG
			printf ("\nmaximum_latitude = %-3.4f\nminimum_latitude = %-3.4f\nmaximum_longitude = %-3.4f\nminimum_longitude = %-3.4f", 
			request->maximum_latitude, request->minimum_latitude,
			request->maximum_longitude, request->minimum_longitude);
#		endif


	return (IMS_OK);


} /* end of v0_boundaries */


/**************************************************************************
**
** refine_srch - do a one-to-one srch between granule region and user region
**               This routine is skipped if global_search, 
**               or global_granules_only is true,  or granule_list is null  
**
** return  IMS_OK    if successful
**         IMS_ERROR if error occurs
**         IMS_FATAL if system error occurs
**
***************************************************************************/
int v0_spatial__refine_srch (V0_DESC_STRUCT *v0Desc)
{
	V0_GRANULE_LIST *first_ptr, *last_ptr, *curr_ptr, *next_ptr;
	V0_REQUEST_STRUCT *request;
	REGION user_region, granule_region;
	int    i;
	int    overlap;
	int    new_granule_count;
	float  min_lat, max_lat;
	
	DEFAULT_REGION (user_region);

	request = &v0Desc->request;
	curr_ptr = v0Desc->result.curr_dataset->granule_list;
	/*
	** have old_granule_list point to the original granule_list.  Because
	** granule_list will point to the new refined granule result at the end of 
	** this function, we have to keep track the original list to free the 
	** allocated memory at cleanup time.
	*/
	v0Desc->result.curr_dataset->old_granule_list = 
		v0Desc->result.curr_dataset->granule_list;
	new_granule_count = 0;
	first_ptr = last_ptr = next_ptr = (V0_GRANULE_LIST *)NULL;


	if ((v0Desc->request.u_min_lat == -90.0) &&
		 (v0Desc->request.u_max_lat ==  90.0) &&
		 (v0Desc->request.u_min_lon == -180.0) &&
		 (v0Desc->request.u_max_lon ==  180.0)) 
	{
		v0Desc->result.curr_dataset->granule_list = curr_ptr;

#		ifdef SDEBUG
			printf ("\nGlobal search specified.  Skip refined search.");
#		endif

		return (IMS_OK);
	}



	while ( curr_ptr != (V0_GRANULE_LIST *)NULL)
	{
		next_ptr = curr_ptr->next_p;

		DEFAULT_REGION (granule_region);
		overlap = 0;

#		ifdef SDEBUG
			printf ("\n\nCheck %s.", curr_ptr->granule_id);
#		endif

		switch (request->region_type)
		{
			case POINT_LOC:

				user_region.geographic_type = POINT;
				user_region.latitude[0] = v0Desc->request.u_lat[0];
				user_region.longitude[0] = v0Desc->request.u_lon[0];
				user_region.projection = DEFAULT_PROJECTION;
				user_region.pole_included  = request->pole_included;

				switch (v0Desc->result.curr_dataset->spatial_type)
				{
					case 1:
					
						granule_region.geographic_type = POINT;
						granule_region.latitude[0] = curr_ptr->center_lat;
						granule_region.longitude[0] = curr_ptr->center_lon;
						granule_region.projection = DEFAULT_PROJECTION;


						overlap = spatial_query_routines__point_in_point
							(user_region, granule_region);

						break;

					case 2:
		
						overlap = 1;
						break;

					case 4:

						granule_region.geographic_type = POLYGON;
					
						/*
						** the four corner points are in clockwise order.  In
						** order to be consistant with v0, we choose the northwest
						** corner as the first point.  NEAR_END is the northwest
						** when ascending frames, and FAR_START is the northwest for
						** decending ones.
						*/
						if (strcmp (curr_ptr->asc_desc, "A") == 0)
						{
							granule_region.latitude[0]  = curr_ptr->ne_lat;
							granule_region.longitude[0] = curr_ptr->ne_lon;
							granule_region.latitude[1]  = curr_ptr->fe_lat;
							granule_region.longitude[1] = curr_ptr->fe_lon;
							granule_region.latitude[2]  = curr_ptr->fs_lat;
							granule_region.longitude[2] = curr_ptr->fs_lon;
							granule_region.latitude[3]  = curr_ptr->ns_lat;
							granule_region.longitude[3] = curr_ptr->ns_lon;
						}
						else if (strcmp (curr_ptr->asc_desc, "D") == 0)
						{
							granule_region.latitude[0]  = curr_ptr->fs_lat;
							granule_region.longitude[0] = curr_ptr->fs_lon;
							granule_region.latitude[1]  = curr_ptr->ns_lat;
							granule_region.longitude[1] = curr_ptr->ns_lon;
							granule_region.latitude[2]  = curr_ptr->ne_lat;
							granule_region.longitude[2] = curr_ptr->ne_lon;
							granule_region.latitude[3]  = curr_ptr->fe_lat;
							granule_region.longitude[3] = curr_ptr->fe_lon;
						}

						max_lat = curr_ptr->max_lat;
						min_lat = curr_ptr->min_lat; 
	
						granule_region.projection = DEFAULT_PROJECTION;
	
						granule_region.pole_included = curr_ptr->pole_included[0];
	
						overlap = spatial_query_routines__point_in_polygon
							(user_region, granule_region);

						break;

					case 0:
					case 3:
						/*
						** no-op for now
						*/
						break;
					default:
						(void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
							"v0_imsSpatial__refine_srch: Invalid spatial_type. ");
						return (IMS_FATAL);

						break;
				}

				break;
	
			case RANGE_LOC:

				/*
				** It is assumed that v0 does not accept rectangular 
				** crossing North/South pole.  If it does, this code will 
				** have to be revisited.  -jlw-
				**
				*/

				user_region.geographic_type = WORLD;
				user_region.latitude[0] = v0Desc->request.u_min_lat;
				user_region.latitude[1] = v0Desc->request.u_max_lat;
				user_region.longitude[0] = v0Desc->request.u_min_lon;
				user_region.longitude[1] = v0Desc->request.u_max_lon;
				user_region.projection = DEFAULT_PROJECTION;
				user_region.pole_included  = request->pole_included;

	
				switch (v0Desc->result.curr_dataset->spatial_type)
				{
					case 1:
					
						/*
						** more filtering needed for POINT granules.  Our query
						** somehow collect all point data within the latitude range
						**  around the globe in pass 1
						**
						*/
						if ((curr_ptr->center_lon<=request->maximum_longitude)&&
							 (curr_ptr->center_lon>=request->minimum_longitude))
						{
							overlap = 1;
						}
						else
						{
							overlap = 0;
						}

						break;

					case 2:
					case 4:
						/*
						** if user select a rectangular region, the second pass 
						** is eliminated, because a rectangular region is identical
						** to its boundaries
						*/

						overlap = 1;
						break;

					case 0:
					case 3:
						/*
						** no-op for now
						*/
						break;

					default:
						(void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
							"v0_imsSpatial__refine_srch: Invalid spatial_type. ");
						return (IMS_FATAL);

						break;
				}

				break;

			case POLYGON_LOC:
	
				user_region.geographic_type = POLYGON;
				user_region.projection = DEFAULT_PROJECTION;
				user_region.pole_included  = request->pole_included;

				for (i=0; i<PT_MAX; i++)
				{
					user_region.latitude[i] = v0Desc->request.u_lat[i];
					user_region.longitude[i] = v0Desc->request.u_lon[i];
				}
	
				switch (v0Desc->result.curr_dataset->spatial_type)
				{
					case 1:
					
						/*
						** spatial_query_routines picks 'all' point data within
						** the latitude range, and ignore the longitude constrains.
						** Our workaround here may still pick up some nearby point 
						** granules which is not enclosed in the polygon user region,
						** but the error rate is smaller.
						** 
						*/

						if (request->cross_180)
						{
							if ((curr_ptr->center_lon<=request->maximum_longitude)||
								 (curr_ptr->center_lon>=request->minimum_longitude))
							{
								overlap = 1;
							}
							else
							{
								overlap = 0;
							}

						}
						else
						{
							if ((curr_ptr->center_lon<=request->maximum_longitude)&&
								 (curr_ptr->center_lon>=request->minimum_longitude))
							{
								overlap = 1;
							}
							else
							{
								overlap = 0;
							}
						}

						break;

					case 2:
		
						if ((curr_ptr->south_lat == -90.0) &&
							 (curr_ptr->north_lat == 90.0) &&
							 (curr_ptr->west_lon  == -180.0) &&
							 (curr_ptr->east_lon  ==  180)) 
						{
							overlap = 1;
						}
		
						else
						{
							granule_region.geographic_type = WORLD;
							granule_region.latitude[0] = curr_ptr->south_lat;
							granule_region.latitude[1] = curr_ptr->north_lat;
							granule_region.longitude[0] = curr_ptr->west_lon;
							granule_region.longitude[1] = curr_ptr->east_lon;
	
							granule_region.projection = DEFAULT_PROJECTION;
	
							overlap = spatial_query_routines__do_regions_overlap
									(user_region, granule_region);
						}

						break;

					case 4:

						granule_region.geographic_type = POLYGON;
					
						if (strcmp (curr_ptr->asc_desc, "A") == 0)
						{
							granule_region.latitude[0]  = curr_ptr->ne_lat;
							granule_region.longitude[0] = curr_ptr->ne_lon;
							granule_region.latitude[1]  = curr_ptr->fe_lat;
							granule_region.longitude[1] = curr_ptr->fe_lon;
							granule_region.latitude[2]  = curr_ptr->fs_lat;
							granule_region.longitude[2] = curr_ptr->fs_lon;
							granule_region.latitude[3]  = curr_ptr->ns_lat;
							granule_region.longitude[3] = curr_ptr->ns_lon;
						}
						else if (strcmp (curr_ptr->asc_desc, "D") == 0)
						{
							granule_region.latitude[0]  = curr_ptr->fs_lat;
							granule_region.longitude[0] = curr_ptr->fs_lon;
							granule_region.latitude[1]  = curr_ptr->ns_lat;
							granule_region.longitude[1] = curr_ptr->ns_lon;
							granule_region.latitude[2]  = curr_ptr->ne_lat;
							granule_region.longitude[2] = curr_ptr->ne_lon;
							granule_region.latitude[3]  = curr_ptr->fe_lat;
							granule_region.longitude[3] = curr_ptr->fe_lon;
						}

						granule_region.projection = DEFAULT_PROJECTION;
			
						granule_region.pole_included = curr_ptr->pole_included[0];
	
						overlap = spatial_query_routines__do_regions_overlap
							(user_region, granule_region);

						break;

					case 0:
					case 3:
						/*
						** no-op for now
						*/
						break;
					default:

						(void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
							"v0_imsSpatial__refine_srch: Invalid spatial_type. ");
						return (IMS_FATAL);

						break;
	
				}	

			default:
	
				break;

		} /* end of switch user region_type  */
	

		if (overlap)
		{
			new_granule_count++;

			if (new_granule_count ==1 )
			{
				first_ptr = curr_ptr;
				last_ptr  = curr_ptr;
			}

			else
			{
				last_ptr->next_p = curr_ptr;
				last_ptr = last_ptr->next_p;
			}


#			ifdef SDEBUG
				printf("\n###The user area overlays with %s\n", 
					curr_ptr->granule_id);
#			endif


			curr_ptr = next_ptr;

		} /* end of if overlap */

		else
		{

			free (curr_ptr);
			curr_ptr = next_ptr;

		} /* end of if not overlap */


	} /* while curr_ptr is not null */

	if (first_ptr != (V0_GRANULE_LIST *)NULL)
	{
		last_ptr->next_p = (V0_GRANULE_LIST *)NULL;
	}

	if ((request->region_type == POINT_LOC) ||
		 (request->region_type == RANGE_LOC) ||
		 (request->region_type == POLYGON_LOC))
	{

		v0Desc->result.curr_dataset->granule_list  = first_ptr;
		v0Desc->result.curr_dataset->granule_count = new_granule_count;

	}

	return (IMS_OK);

} /* end of refine_srch */

/**************************************************************************
** 
** longitude_conversion -- convert longitude values 
**	        direction 1: from -180/180 to 0/360
**         direction 2: from 0/360 to -180/180
**
***************************************************************************/

float longitude_conversion (float lon, int direction)
{

	if (direction == 1)
	{
		if (lon < 0.0)
		{
			lon = lon + 360.0;
		}
	}
	else
	{
		if (lon > 180.00)
		{
			lon = lon - 360.0;
		}
	}

	return lon;

}/* end of longitude_conversion */

/**************************************************************************
** 
** rectTo4pt -- converts the geographical representation of a rectangle into  
**              four points 
**
***************************************************************************/
int v0_spatial__rectTo4pt (float *max_lat, float *min_lat, 
                           float *max_lon, float *min_lon,
                           float *nw_lat,  float *nw_lon,
                           float *ne_lat,  float *ne_lon,
                           float *se_lat,  float *se_lon,
                           float *sw_lat,  float *sw_lon)
{

	*nw_lat = *max_lat;
	*nw_lon = *min_lon; 
	*ne_lat = *max_lat;
	*ne_lon = *max_lon;
	*se_lat = *min_lat;
	*se_lon = *max_lon;
	*sw_lat = *min_lat;
	*sw_lon = *min_lon;

	return (IMS_OK);

} /* end of rectTo4pt */

