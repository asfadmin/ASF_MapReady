#include "asf.h"
#include "proj.h"
#include "ddr.h"
#include "asf_meta.h"

int geoLatLon(double eleva, char *in_meta, char *in_ddr, char *out_tps)
{
	int x,y,k;			/* Loop counters */
	FILE *out; 			/*Output tie point file */
	meta_parameters *meta=NULL;
	struct DDR ddr;
	inverse_transform ddr_trans;/*Geocoding projection to lat,lon function pointer*/
	

	/* Print the user specified average elevation of the output area 
	-----------------------------------------------------------*/
	printf("   Input average output elevation = %f\n\n",eleva);
	if (logflag) {
	  sprintf(logbuf, "   Input average output elevation = %f\n\n",eleva);
	  printLog(logbuf);
	}
	
	out=FOPEN(out_tps,"w");/*Open ascii output file*/
	
	/* Initialize metadata
	-----------*/
	c_getddr(in_ddr,&ddr);

/*Try to read from SAR metadata if at all possible*/
	if (extExists(in_meta,".meta")||
		extExists(in_meta,".L")||
		extExists(in_meta,".ldr"))
	{
/*		printf("Reading .meta metadata...\n");*/
		meta=meta_init(in_meta);
	}
	else if (ddr.valid[DDPCV]!=0)
	/*Read projection information from DDR*/
	{
		inverse_transform trans[100];
		int error=0;
/*		printf("Initializing LAS Projection from DDR, since no .meta found...\n");*/
		inv_init(ddr.proj_code,ddr.zone_code,ddr.proj_coef,
			ddr.datum_code,NULL,NULL,&error,trans);
		if (error!=0) {
		  sprintf(errbuf, "   ERROR: %d initializing LAS projection!\n",error);
		  printErr(errbuf);
		}
		ddr_trans=trans[ddr.proj_code];
	} else { 
	/*No valid metadata found-- bail!*/
		sprintf(errbuf, "   ERROR: No valid metadata found for file '%s'!\n"
			"geoLatLon exiting...\n",in_meta);
		printErr(errbuf);
	}
	
	/* Create grid over image
	-----------*/
	
#define REZ_X 8
#define REZ_Y 8
	for(y=0; y<=REZ_Y; y++)
		for(x=0; x<=REZ_X; x++,k++)
		{
			double lat,lon;	/* Output latitude and longitude  */
			int img_x,img_y; /*Coordinates in DDR image*/
			
			img_y=y*ddr.nl/REZ_Y;
			img_x=x*ddr.ns/REZ_X;
			if (meta!=NULL)
			{/*Is a Metadata image*/
				int meta_x,meta_y; /*Coordinates in original image*/
				/*Figure out the coordinates in the original image*/
				meta_get_orig((void *)&ddr,img_y,img_x,&meta_y,&meta_x);
				/*Convert these coordinates to lat/lon*/
				meta_get_latLon(meta,meta_y,meta_x,eleva,&lat,&lon);
			} else {/*Is just a LAS image--use ddr_trans*/
				double projX,projY;
				projX=ddr.upleft[1]+((double)img_x)/ddr.ns*(ddr.upright[1]-ddr.upleft[1]);
				projY=ddr.upleft[0]+((double)img_y)/ddr.nl*(ddr.loleft[0]-ddr.upleft[0]);
				ddr_trans(projX,projY,&lon,&lat);
				lon*=R2D;
				lat*=R2D;
			}
			
			/*Print out the lat & lon information to output file*/
			fprintf(out,"%.12f %.12f %d %d\n",lat,lon,img_x,img_y);
		}
	
	FCLOSE(out);
	
/*	printf("geoLatLon complete\n");*/
	return(0);
}
