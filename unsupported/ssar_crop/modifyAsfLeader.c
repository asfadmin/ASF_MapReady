/****************************************************************
FUNCTION NAME: modifyAsfLeader - creates a copy of a leader file w/ changes

SYNTAX:
    modifyAsfLeader(char *ceosFile,int topLine,int onl,int leftSamp,int onp,
		char *ofile,
		int o_tlc_line,int o_tlc_samp,int o_trc_line,int o_trc_samp,
                int o_blc_line,int o_blc_samp,int o_brc_line,int o_brc_samp)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    ceosFile	char * 		Input leader file to copy
    topLine	int		input line clipped to
    onl		int		output number of lines
    leftSamp	int		input left sample clipped to
    onp		int		output number of samples
    ofile	char *		name of output leader file to create
    o_tlc_line  int		New top left corner
    o_tlc_samp  int
    o_trc_line  int		New top right corner
    o_trc_samp  int
    o_blc_line  int		New bottom left corner
    o_blc_samp  int
    o_brc_line  int		New bottom right corner
    o_brc_samp  int

DESCRIPTION:

    Reads ceosFile.L, and writes ofile.L, modifying certain records as it
    goes.

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ceos.h"
#include "caplib.h"
#include "const.h"

double mag(double *vec);
double dot(double *u, double *v);
double calc_distance(double R,double lon0,double lat0,double lon1,double lat1);

void ssar_tool_init(char *inSAR, int nl,int ns);
void ll_to_proj(double lat_d,double lon,double *op1,double *op2);
void proj_to_ll(double op1,double op2, double *lat,double *lat_d,double *lon);

void modify_dssr(struct dataset_sum_rec *d,int topLine, int leftSamp);
void modify_mpdr(struct VMPDREC *mpdr, char *ceosFile, int topLine,
		 int leftSamp, int onl, int onp);
void modify_facdr(struct VFDRECV *facdr,int onp, int onl, 
	int o_tlc_line,int o_tlc_samp,int o_trc_line,int o_trc_samp,
        int o_blc_line,int o_blc_samp,int o_brc_line,int o_brc_samp);

extern double proj_x, proj_y, pd_x, pd_y;

void modifyAsfLeader(char *ceosFile,int topLine,int onl,int leftSamp,int onp,
		char *ofile,
		int o_tlc_line,int o_tlc_samp,int o_trc_line,int o_trc_samp,
                int o_blc_line,int o_blc_samp,int o_brc_line,int o_brc_samp)
{
  unsigned char buff[20000];
  int    itype, length;
  struct HEADER bufhdr;
  int era;
  char lfile[256];
  FILE *fp, *fpo;

  struct VFDRECV facdr;           /* Facility data record         */
  struct VMPDREC mpdr;            /* Map projection data record   */
  struct dataset_sum_rec dssr;    /* Data Set Summary Record      */

  era = set_era(ceosFile,lfile,1);
  fp = FOPEN(lfile, "rb");
  strcpy(lfile,ofile);
  strcat(lfile,".L"); 
  fpo = FOPEN(lfile,"wb");

  while (1)
   {
     if (fread(&bufhdr,sizeof(bufhdr),1,fp)!=1) break;
     itype = bufhdr.rectyp[1];
     length = bigInt32(bufhdr.recsiz) - 12;
     FREAD(buff,length,1,fp);
     FWRITE(&bufhdr,sizeof(bufhdr),1,fpo);
     switch (itype)
      {
        case (10):
		 /* printf("Data Set Summary Record.\n"); */
		 get_dssr(ceosFile,&dssr);
		 modify_dssr(&dssr,topLine,leftSamp);
		 Code_DSSR(buff,&dssr,era,toASCII);
		 FWRITE(&buff[12],length,1,fpo);
		 break;
        case (11): /* printf("Data Record.\n");*/ break; 
        case (20):
		 /*printf("Map Projection Data Record.\n");*/
		 get_mpdr(ceosFile,&mpdr);
		 modify_mpdr(&mpdr,ceosFile,topLine,leftSamp,onl,onp);
		 Code_MPDR(buff,&mpdr,toASCII);
		 FWRITE(&buff[12],length,1,fpo);
		 break;
        case (30):
		 /*printf("Platform Position Data Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (40):
		 /*printf("Attitude Data Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (50):
		 /*printf("Radiometric Data Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (51):
		 /*printf("Radiometric Compensation Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (60):
		 /*printf("Data Quality Summary Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (70):
		 /*printf("Data Histograms Record.\n"); */
		 FWRITE(buff,length,1,fpo);
	 	 break;
        case (80):
		 /*printf("Range Spectra Record.\n"); */
		 FWRITE(buff,length,1,fpo);
		 break;
        case (90):
		 /*printf("Digital Elevation Model Descriptor Rec.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (100):
		 /*printf("Radar Parameter Data Update Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (110):
		 /*printf("Annotation Data Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (120):
		 /*printf("Detailed Processing Parameters Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (130):
		 /*printf("Calibration Data Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (140):
		 /*printf("Ground Control Points Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (192):
		 /*printf("File Descriptor Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (200):
		 /*printf("Facility Related Data Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (201):
		 /*printf("GPS Metadata Record.\n");*/
		 FWRITE(buff,length,1,fpo);
		 break;
        case (210):
		 /*printf("Facility Related Data Record (RADARSAT).\n");*/
		 get_facdr(ceosFile,&facdr);
		 modify_facdr(&facdr,onp,onl, 
		 	 o_tlc_line,o_tlc_samp,o_trc_line,o_trc_samp,
	       		 o_blc_line,o_blc_samp,o_brc_line,o_brc_samp);
		 Code_FACDR(buff,&facdr,era,toASCII);
		 FWRITE(&buff[12],length,1,fpo);
		 break;
        default: printf("Not Valid Record Type: %d\n",itype);
      }

   }
   
   FCLOSE(fp);
   FCLOSE(fpo);

}

/* Based on the given corner positions, recalculates corner geolocations */
void modify_facdr(struct VFDRECV *facdr,int onp, int onl, 
	int o_tlc_line,int o_tlc_samp,int o_trc_line,int o_trc_samp,
        int o_blc_line,int o_blc_samp,int o_brc_line,int o_brc_samp)
{
   double x,y,dum1,lat,lon/*,lat0,lon0*/,R,D;

   R = facdr->eradcntr;

   /* Change FACDR fields
    --------------------*/
   facdr->npixels = onp;
   facdr->apixels = onp;
   facdr->nlines = onl;
   facdr->alines = onl;
  
   /* Convert corner line,samples to corner lat,lon
    ----------------------------------------------*/
   printf(" Geolocations of Corners:\n");
   y = o_tlc_line*pd_y+proj_y;
   x = o_tlc_samp*pd_x+proj_x;
   proj_to_ll(x,y,&dum1,&lat,&lon);
   printf("\tTop Left Corner    : %f, %f\n",lat,lon);
   D = calc_distance(R,lat,lon,facdr->nearelat,facdr->nearelon);
   printf("\tDistance Moved     : %f\n",D);
   facdr->nearelat = lat; facdr->nearelon = lon;

   y = o_trc_line*pd_y+proj_y;
   x = o_trc_samp*pd_x+proj_x;
   proj_to_ll(x,y,&dum1,&lat,&lon);
   printf("\tTop Right Corner   : %f, %f\n",lat,lon);
   D = calc_distance(R,lat,lon,facdr->farelat,facdr->farelon);
   printf("\tDistance Moved     : %f\n",D);
   facdr->farelat = lat; facdr->farelon = lon;

   y = o_blc_line*pd_y+proj_y;
   x = o_blc_samp*pd_x+proj_x;
   proj_to_ll(x,y,&dum1,&lat,&lon);
   printf("\tBottom Left Corner : %f, %f\n",lat,lon);
   D = calc_distance(R,lat,lon,facdr->nearslat,facdr->nearslon);
   printf("\tDistance Moved     : %f\n",D);
   facdr->nearslat = lat; facdr->nearslon = lon;
   
   y = o_brc_line*pd_y+proj_y;
   x = o_brc_samp*pd_x+proj_x;
   proj_to_ll(x,y,&dum1,&lat,&lon);
   printf("\tBottom Right Corner: %f, %f\n",lat,lon);
   D = calc_distance(R,lat,lon,facdr->farslat,facdr->farslon);
   printf("\tDistance Moved     : %f\n",D);
   facdr->farslat = lat; facdr->farslon = lon;
}

/* Change position of scene center line,sample */
void modify_dssr(struct dataset_sum_rec *d,int topLine, int leftSamp)
 {
   d->sc_lin -=topLine;
   d->sc_pix -=leftSamp;
 }

/* Based on the new topLine and leftSamp, modifies map projection image
   corner locations.  Also fixes UTM and PS projection parameter errors
 ---------------------------------------------------------------------*/
void modify_mpdr(struct VMPDREC *mpdr, char *ceosFile, int topLine, 
		int leftSamp, int onl, int onp)
 {
   int old_nl, old_np;
   double lat,lon,dum1;

   /* Change MPDR fields
    -------------------*/
   old_nl = mpdr->npixels;
   old_np = mpdr->nlines;

   ssar_tool_init(ceosFile,old_nl,old_np);
   /* printf("pd_y, pd_x %lf %lf\n",pd_y,pd_x);*/

   mpdr->npixels = onp;
   mpdr->nlines = onl;

   if (strncmp(mpdr->mpdesig, "GROUND RANGE",12) == 0)
     {
      mpdr->tlcnorth += pd_x*leftSamp;
      mpdr->blcnorth = mpdr->tlcnorth;

      mpdr->trcnorth = mpdr->tlcnorth+onp*pd_x;
      mpdr->brcnorth = mpdr->trcnorth;

      mpdr->tlceast += pd_y*topLine;
      mpdr->trceast = mpdr->tlceast;

      mpdr->brceast = mpdr->tlceast+onl*pd_y;
      mpdr->blceast = mpdr->brceast;

      proj_to_ll(mpdr->tlcnorth,mpdr->tlceast,&dum1,&lat,&lon);
      mpdr->tlclat = lat; mpdr->tlclong = lon;
  
      proj_to_ll(mpdr->trcnorth,mpdr->trceast,&dum1,&lat,&lon);
      mpdr->trclat = lat; mpdr->trclong = lon;
  
      proj_to_ll(mpdr->blcnorth,mpdr->blceast,&dum1,&lat,&lon);
      mpdr->blclat = lat; mpdr->blclong = lon;
  
      proj_to_ll(mpdr->brcnorth,mpdr->brceast,&dum1,&lat,&lon);
      mpdr->brclat = lat; mpdr->brclong = lon;
     }
   else
     {
      mpdr->tlcnorth += pd_y*topLine;
      mpdr->trcnorth = mpdr->tlcnorth;

      mpdr->blcnorth = mpdr->tlcnorth+onl*pd_y;
      mpdr->brcnorth = mpdr->blcnorth;

      mpdr->tlceast  += pd_x*leftSamp;
      mpdr->blceast  = mpdr->tlceast;

      mpdr->trceast  = mpdr->tlceast+onp*pd_x;
      mpdr->brceast  = mpdr->brceast;

      proj_to_ll(mpdr->tlceast,mpdr->tlcnorth,&dum1,&lat,&lon);
      mpdr->tlclat = lat; mpdr->tlclong = lon;

      proj_to_ll(mpdr->trceast,mpdr->trcnorth,&dum1,&lat,&lon);
      mpdr->trclat = lat; mpdr->trclong = lon;

      proj_to_ll(mpdr->blceast,mpdr->blcnorth,&dum1,&lat,&lon);
      mpdr->blclat = lat; mpdr->blclong = lon;

      proj_to_ll(mpdr->brceast,mpdr->brcnorth,&dum1,&lat,&lon);
      mpdr->brclat = lat; mpdr->brclong = lon;

      if (strncmp(mpdr->mpdesig, "PS-SMM/I", 8) == 0)
       {
	mpdr->upslong = -45.0;
       }
      else if (strncmp(mpdr->mpdesig, "UTM", 3) == 0)
       {
	int zone;
	zone = (int) ((mpdr->utmpara1+180.0)/6.0 + 1.0);
 	sprintf(mpdr->utmzone,"%i",zone);	
       }
      else if (strncmp(mpdr->mpdesig, "LAMBERT", 7) == 0) 
       {
        printf("WARNING: Lambert-geocoded images may not be\n"
                "accurately geocoded!\n");
       } 


     }

}

double mag(double *vec) {return(sqrt(SQR(vec[0])+SQR(vec[1])+SQR(vec[2])));}
double dot(double *u, double *v) {return(u[0]*v[0]+u[1]*v[1]+u[2]*v[2]); }

double calc_distance(double R, double lon0, double lat0,
			       double lon1, double lat1)
 {
   double vec0[3], vec1[3]/*, diff[3]*/;
   double distance;

   vec0[0] = R * cosd(lon0) * cosd(lat0);
   vec0[1] = R * sind(lon0) * cosd(lat0);
   vec0[2] = R * (1-ECC2) * sind(lat0);

   vec1[0] = R * cosd(lon1) * cosd(lat1);
   vec1[1] = R * sind(lon1) * cosd(lat1);
   vec1[2] = R * (1-ECC2) * sind(lat1);

   distance = R * acos(dot(vec0, vec1)/(mag(vec0)*mag(vec1)));

   return(distance);
 }

