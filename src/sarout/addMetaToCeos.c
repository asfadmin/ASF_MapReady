/****************************************************************
FUNCTION NAME:

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/
#include "asf.h"
#include "ddr.h"
#include "sarout.h"
#include "asf_meta.h"
#include "ifm.h"

void calcNadir(ceosLeader *, stateVector *);
void calcGeoref(ceosLeader *, meta_parameters *, int, int);
void calcSceneSizes(ceosLeader *, meta_parameters *, struct DDR *);
void calcHeadings(ceosLeader *, stateVector,stateVector,stateVector);
void calcSquint(ceosLeader *data, meta_parameters *meta, double centerTime);
void fill_times(meta_parameters *,double ,ceosLeader *);
double getSRfromGR(stateVector,double,double,double);
void fill_processor(ceosLeader *,meta_parameters *);
void fill_name(meta_parameters *meta, struct DDR *ddr, ceosLeader *data, int mode);
void inc_time(int inYear,int inJulDay, double inSecond, double inInc,
	 int *outYear, int *outMonth, int *outDay, int *outHour,
	 int *outMin, double *outSec);
void fill_mpdr(meta_parameters *meta, struct DDR *ddr, ceosLeader *data);
void fill_dssr(meta_parameters *meta, struct DDR *ddr, double centerTime,
	  struct dataset_sum_rec *dssr);
void fill_ppdr(meta_parameters *meta, struct DDR *ddr,
	 stateVector *sVec, stateVector *cVec, stateVector *eVec,
	 struct pos_data_rec *ppdr);
void fill_facdr(meta_parameters *meta, struct DDR *ddr, 
	stateVector *cVec, double centerTime, struct VFDRECV *facdr);
int gmt2day(int year, int gmt_day);
int gmt2month(int year, int gmt_day);


#define ECC_E   	8.1827385E-2       /* Eccentricity of the earth  */
#define ECC2    	(ECC_E*ECC_E)      /* Eccentricity squared       */
#define WE      	0.00417807442      /* Earth Rotation Rate (deg/sec) */
#define	atand(x) 	(atan(x)*R2D)
#define	tand(x) 	tan((x)*D2R)

double imageTimeLen;

void addMetaToCeos(meta_parameters *meta, struct DDR *ddr, ceosLeader *data, int mode)
 {
    double dt;
    double startTime, centerTime, endTime;
    stateVector	  cVec, eVec, sVec;
 
    if (meta->stVec == NULL)
      { 
 	printf("Unable to determine metadata without state vectors\n");
	exit(0);
      }

    dt = meta->geo->timeShift + ddr->master_line * meta->geo->azPixTime; 
    imageTimeLen=meta->geo->azPixTime * ddr->nl*(ddr->pdist_y/meta->geo->yPix);
    data->dssr.prf =
    data->dssr.prf1 = 
    data->facdr.prfreq = meta->info->prf;

    if (meta->geo->type == 'P')
      {
	imageTimeLen = meta->stVec->vecs[meta->stVec->num-1].time;
      }

    startTime = dt;
    centerTime = imageTimeLen / 2.0 + dt;
    endTime = imageTimeLen + dt;

    sVec = meta_get_stVec(meta,startTime);
    cVec = meta_get_stVec(meta,centerTime); 
    eVec = meta_get_stVec(meta,endTime);

    fill_processor(data,meta);

    calcNadir(data, &cVec);
    calcSceneSizes(data, meta, ddr);
    calcHeadings(data, sVec, cVec, eVec);
    fill_times(meta, centerTime, data);

    fill_dssr(meta, ddr, centerTime, &(data->dssr));
    fill_ppdr(meta, ddr, &sVec, &cVec, &eVec, &(data->ppdr));
    fill_facdr(meta, ddr, &cVec, centerTime, &(data->facdr));

    calcGeoref(data, meta, ddr->nl, ddr->ns);
    calcSquint(data, meta, centerTime);

    fill_name(meta, ddr, data, mode);

    if (meta->geo->type == 'P')
      {
        printf("Adding map projection data record\n");
        fill_mpdr(meta, ddr, data); 
      }
 }

void fill_name(meta_parameters *meta, struct DDR *ddr, ceosLeader *data, int mode)
 {
   char name[64];
   char tmp[64];

   /* Add Platform Name */
   if (strncmp(meta->info->sensor,"ERS1",4)==0) sprintf(tmp,"E1");
   else if (strncmp(meta->info->sensor,"ERS2",4)==0) sprintf(tmp,"E2");
   else if (strncmp(meta->info->sensor,"JERS1",4)==0) sprintf(tmp,"J1");
   else if (strncmp(meta->info->sensor,"RSAT-1",4)==0) sprintf(tmp,"R1");

   /* Add Orbit number */
   sprintf(name,"%2s%.5i",tmp,meta->info->orbit);

   /* Add frame number */
   strcat(name,"999");  /* fake frame number */

   /* Add map projection type */
   if (meta->geo->type == 'P')
    {
      if (meta->geo->proj->type == 'A') strcat(name,"G");
      else strcat(name,&meta->geo->proj->type); 
    }
   else strcat(name,&meta->geo->type);

   /* Add data pixel spacing */
   if (meta->geo->type == 'S') strcat(name,"0");
   else
    {
     if (ddr->pdist_x == 6.25) strcat(name,"0");
     else if (ddr->pdist_x == 12.5) strcat(name,"1");
     else if (ddr->pdist_x == 25.0) strcat(name,"2");
     else if (ddr->pdist_x == 50.0) strcat(name,"3");
     else if (ddr->pdist_x == 100.0) strcat(name,"4");
     else if (ddr->pdist_x == 200.0) strcat(name,"5");
     else if (ddr->pdist_x == 400.0) strcat(name,"6");
     else if (ddr->pdist_x == 800.0) strcat(name,"7");
     else if (ddr->pdist_x == 1600.0) strcat(name,"8");
     else strcat(name,"9");
    }

   /* Add processing mode */
   if (mode == CEOS_CCSD) strcat(name,"C");
   else if (mode == CEOS_SLC) strcat(name,"X");
   else strcat(name,"S");

   /* Add version number */ 
   strcat(name,"999");

   strncpy(data->facdr.imageid,name,12);
   strcpy(data->dssr.product_id, name);

 }


void calcSquint(ceosLeader *data, meta_parameters *meta, double centerTime)
 {
    GEOLOCATE_REC *g;
    stateVector stVec=meta_get_stVec(meta,centerTime);
    double        look, yaw;

    g=init_geolocate_meta(&stVec,meta);
    getLookYaw(g,
               meta_get_slant(meta,data->dssr.sc_lin,data->dssr.sc_pix),
               meta_get_dop(meta,data->dssr.sc_lin,data->dssr.sc_pix),
               &look,&yaw);
    free_geolocate(g);
    data->facdr.squintan = sin(look)*sin(yaw);
 }




void fill_processor(ceosLeader *data,meta_parameters *meta)
 {
   char tmpStr[20];
   char *ptr;
   int  i,j;
  
   ptr = meta->info->processor;

   i=0; j=0;
   while (ptr[i] != '/') tmpStr[j++] = ptr[i++];
   tmpStr[j] = '\0';
   strcpy(data->dssr.fac_id,tmpStr);

   i++; j=0;
   while (ptr[i] != '/') tmpStr[j++] = ptr[i++];
   tmpStr[j] = '\0';
   strcpy(data->dssr.sys_id,tmpStr);

   i++; j=0;
   while (i<strlen(meta->info->processor)) tmpStr[j++] = ptr[i++];
   tmpStr[j] = '\0';
   strcpy(data->facdr.procvers,tmpStr);
   strcpy(data->dssr.ver_id,tmpStr);
 }

void calcHeadings(ceosLeader *data, stateVector sVec, stateVector cVec, 
	     stateVector eVec)
 {
    double xx, yy, zz, d1, d2, tmp1;
    double len, sc_dist;

    if (cVec.vel.z >= 0.0) 
      {
	strcpy(data->dssr.asc_des,"ASCENDING");
	strcpy(data->facdr.ascdesc,"A");
      }
    else
      {
	strcpy(data->dssr.asc_des,"DESCENDING");
	strcpy(data->facdr.ascdesc,"D");
      }

    /* Calculate the processed scene center heading 
     ---------------------------------------------*/
    xx = SQR(sVec.pos.x); yy = SQR(sVec.pos.y); zz = SQR(sVec.pos.z);
    d1 = (xx+yy)/(xx+yy+zz);
    d1 = acos(sqrt(d1));
    len = sqrt(xx+yy+zz);

    xx = SQR(eVec.pos.x); yy = SQR(eVec.pos.y); zz = SQR(eVec.pos.z);
    d2 = (xx+yy)/(xx+yy+zz);
    d2 = acos(sqrt(d2));

    xx = sVec.pos.x - eVec.pos.x;
    yy = sVec.pos.y - eVec.pos.y;
    zz = sVec.pos.z - eVec.pos.z;
    sc_dist = sqrt(xx*xx + yy*yy + zz*zz);

    tmp1 = RTD*asin((len*fabs(d2-d1))/sc_dist);
    if (cVec.vel.z >= 0.0) tmp1 = 270 + tmp1;
    else                   tmp1 = 270 - tmp1;

    data->dssr.pro_head = tmp1;


    /* Calculate the platform heading/track angle
     -------------------------------------------*/
    xx = SQR(sVec.pos.x)+SQR(sVec.pos.y); 
    yy = xx + SQR(sVec.pos.z);
    d1 = acos(sqrt(xx/yy));
 
    xx = SQR(eVec.pos.x)+SQR(eVec.pos.y);
    yy = xx + SQR(eVec.pos.z);
    d2 = acos(sqrt(xx/yy));

    len = data->facdr.swazim;
    tmp1 = RTD*asin((data->facdr.eradnadr*fabs(d2-d1))/len);

    if (cVec.vel.z >= 0.0) tmp1 = 270 + tmp1;
    else                   tmp1 = 270 - tmp1;

    data->dssr.plat_head_scene = tmp1;
    data->facdr.trackang = tmp1;

    /*-- THIS IS THE RIGHT WAT TO DO IT-------
    tmp1 = RTD*getHeading(scVec[1]);
    if (tmp1 < 0.0) tmp1 += 360.0;
    -----------------------------------------*/

}   

void fill_mpdr(meta_parameters *meta, struct DDR *ddr, ceosLeader *data)
 {
    double lat,lon,tmp;
    proj_parameters *p = meta->geo->proj;

    data->mpdr.npixels  = ddr->ns;
    data->mpdr.nlines   = ddr->nl;
    data->mpdr.nomipd   = ddr->pdist_x;
    data->mpdr.nomild   = ddr->pdist_y;
    data->mpdr.orbitincl= data->ppdr.orbit_ele[2];
    data->mpdr.ascnode  = data->ppdr.orbit_ele[3];
    data->mpdr.distplat = sqrt( SQR(data->ppdr.pos_vec[1][0])+
				SQR(data->ppdr.pos_vec[1][1])+
				SQR(data->ppdr.pos_vec[1][2]))*1000.0;
    data->mpdr.altplat  = data->facdr.scalt*1000.0;
    data->mpdr.velnadir = data->facdr.swathvel;
    data->mpdr.plathead = data->facdr.trackang;

    data->mpdr.tlcnorth = p->startY;
    data->mpdr.tlceast  = p->startX;
    data->mpdr.trcnorth = p->startY;
    data->mpdr.trceast  = p->startX + ddr->ns * p->perX;
    data->mpdr.brcnorth = p->startY + ddr->nl * p->perY;
    data->mpdr.brceast  = p->startX + ddr->ns * p->perX;
    data->mpdr.blcnorth = p->startY + ddr->nl * p->perY;
    data->mpdr.blceast  = p->startX;

    if (p->type == 'A')
      {
	strcpy(data->mpdr.mpdesig,"GROUND RANGE");
	strcpy(data->mpdr.upsdesc,"GROUND RANGE");

 	/* Swap projection to match backwards record */
	tmp = data->mpdr.tlcnorth;
	data->mpdr.tlcnorth = data->mpdr.tlceast;
	data->mpdr.tlceast = tmp;
	tmp = data->mpdr.trcnorth;
	data->mpdr.trcnorth = data->mpdr.trceast;
	data->mpdr.trceast = tmp;
	tmp = data->mpdr.brcnorth;
	data->mpdr.brcnorth = data->mpdr.brceast;
	data->mpdr.brceast = tmp;
	tmp = data->mpdr.blcnorth;
	data->mpdr.blcnorth = data->mpdr.blceast;
	data->mpdr.blceast = tmp;
      }
    else if (p->type == 'P')
      {
	strcpy(data->mpdr.mpdesig,"PS-SMM/I");
	strcpy(data->mpdr.upsdesc,"PS-SMM/I");
        data->mpdr.upslong = p->param.ps.slon;
        data->mpdr.upslat = p->param.ps.slat;
        data->mpdr.upsscale = 1.0;
      }
    else if (p->type == 'L')
      {
	strcpy(data->mpdr.mpdesig,"LAMBERT");
	strcpy(data->mpdr.upsdesc,"LAMBERT");
	data->mpdr.nsplong = p->param.lambert.lon0;
	data->mpdr.nsplat  = p->param.lambert.lat0;
        data->mpdr.nsppara1 = p->param.lambert.plat1;
        data->mpdr.nsppara2 = p->param.lambert.plat2;
      }
    else if (p->type == 'U')
      {
	strcpy(data->mpdr.mpdesig,"UTM");
	strcpy(data->mpdr.upsdesc,"UTM");
        sprintf(data->mpdr.utmzone,"%i",p->param.utm.zone);
        data->mpdr.utmpara1 = data->facdr.imgclon;
      }

    meta_get_latLon(meta, 0.0, 0.0, 0.0, &lat, &lon);
    data->mpdr.tlclat = lat;
    data->mpdr.tlclong = lon;
    meta_get_latLon(meta, 0.0, ddr->ns, 0.0, &lat, &lon);
    data->mpdr.trclat = lat;
    data->mpdr.trclong = lon;
    meta_get_latLon(meta, ddr->nl, ddr->ns, 0.0, &lat, &lon);
    data->mpdr.brclat = lat;
    data->mpdr.brclong = lon;
    meta_get_latLon(meta, ddr->nl, 0.0, 0.0, &lat, &lon);
    data->mpdr.blclat = lat;
    data->mpdr.blclong = lon;

 }

void calcSceneSizes(ceosLeader *data, meta_parameters *meta, struct DDR *ddr)
 {
    double calcSwathWidth(meta_parameters *, int);
    double scene_wid, scene_len;

    data->facdr.nlines = ddr->nl;
    data->facdr.alines = ddr->nl;
    data->facdr.npixels = ddr->ns;
    data->facdr.apixels = ddr->ns;
    data->dssr.sc_lin = ddr->nl/2;
    data->dssr.sc_pix = ddr->ns/2;

    if (meta->geo->type!='S') scene_wid = (double) ddr->ns*ddr->pdist_x/1000.0;
    else scene_wid = calcSwathWidth(meta,ddr->ns) / 1000.0; 
    scene_len = (double) ddr->nl * ddr->pdist_y / 1000.0;

    data->dssr.scene_wid = scene_wid;
    data->facdr.swrange = scene_wid;
    data->dssr.scene_len = scene_len;
    data->facdr.swazim = scene_len;

    data->dssr.line_spacing = ddr->pdist_y;
    data->dssr.pixel_spacing = ddr->pdist_x;
    data->facdr.azpixspc = ddr->pdist_y;
    data->facdr.rapixspc = ddr->pdist_x;
 }

void calcGeoref(ceosLeader *data, meta_parameters *meta, int nl, int ns)
 {
    double lat, lon, incdang;

    meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
    data->facdr.nearslon = lon;
    data->facdr.nearslat = lat;

    meta_get_latLon(meta, nl, 0, 0, &lat, &lon);
    data->facdr.nearelon = lon;
    data->facdr.nearelat = lat;

    meta_get_latLon(meta, 0, ns, 0, &lat, &lon);
    data->facdr.farslon = lon;
    data->facdr.farslat = lat;

    meta_get_latLon(meta, nl, ns, 0, &lat, &lon);
    data->facdr.farelon = lon;
    data->facdr.farelat = lat;

    meta_get_latLon(meta, nl/2, ns/2, 0, &lat, &lon);
    data->facdr.imgclat = lat;
    data->facdr.imgclon = lon;
    data->dssr.pro_lat = lat;
    data->dssr.pro_long = lon;

    data->facdr.antlook = RTD * meta_look(meta, nl/2, ns/2);
    incdang = RTD * meta_incid(meta, nl/2, ns/2);
    data->facdr.incedang = incdang;
    data->dssr.incident_ang = incdang;
 }

void calcNadir(ceosLeader *data, stateVector *cVec)
 {
    double Rsc, plat_lat, plat_lon, tmp1, REn, dlat, H;  

    cart2sph(cVec->pos,&Rsc,&plat_lat,&plat_lon);
    plat_lon *= RTD;
    if (plat_lon<-180.0) plat_lon+=360.0;
    tmp1 = plat_lat;                       /* tmp1 is geocentric latitude   */
    plat_lat = atand(tan(tmp1)/(1-ECC2));  /* plat_lat is geodetic latitude */
    REn = (RE*RP)/sqrt(SQR(RP*cos(tmp1)) + SQR(RE*sin(tmp1)));
    dlat = plat_lat*DTR - tmp1;            /* difference in latitudes       */
    H = (Rsc - REn) * cos(dlat);
    plat_lat = plat_lat - (sin(dlat)*H/REn)*RTD;

    data->facdr.eradnadr = REn / 1000.0;
    data->facdr.scalt = H / 1000.0;
    data->dssr.plat_long = plat_lon;
    data->dssr.plat_lat = plat_lat;
 }

void fill_times(meta_parameters *meta, double centerTime, ceosLeader *data)
 {
    int year, month, day, hour, min, millisec;
    double sec;

    inc_time(meta->stVec->year, meta->stVec->julDay, meta->stVec->second,
	     centerTime,&year,&month,&day,&hour,&min,&sec);
    millisec = (int) (1000.0*sec);
    sprintf(data->dssr.inp_sctim,"%.4i%.2i%.2i%.2i%.2i%.5i",
	    year,month,day,hour,min,millisec);
    sprintf(data->facdr.imgyear,"%i",meta->stVec->year);
    sprintf(data->facdr.imgtime,"%.3i:%.2i:%.2i:%06.3f",meta->stVec->julDay,
		hour,min,(float)millisec/1000.0);
 }

/*----------------------------------------------------------------------
  Given:   Year, Julian Day, Second of Day, increment time (in seconds)
  Compute: Year, Month, Day, Hour, Min, Sec after adding increment
----------------------------------------------------------------------*/
void inc_time(int inYear,int inJulDay, double inSecond, double inInc,
	 int *outYear, int *outMonth, int *outDay, int *outHour,
	 int *outMin, double *outSec)
 {
   static int DIM[13]={0,31,28,31,30,31,30,31,31,30,31,30,31};/*Days in Month*/
   int tyear, tmonth, tday, thour, tmin;
   double tsec;
   short ovrflw = 1;

   if (inYear%4==0) DIM[2]=29; /* Check for Leap Year */

   tyear = inYear;
   tmonth = gmt2month(inYear, inJulDay);
   tday = gmt2day(inYear,inJulDay);
   thour=(int)(inSecond/3600);
   tmin=((int)inSecond-3600*thour)/60;
   tsec=inSecond-3600.0*thour-60.0*tmin;

   tsec+=inInc;

   tsec -= (tsec >=60.0) ? 60.0 : (ovrflw=0);
   if (ovrflw) tmin   += (tmin == 59) ? -59 : ovrflw--;
   if (ovrflw) thour  += (thour == 23) ? -23 : ovrflw--;
   if (ovrflw) tday   += (tday==DIM[tmonth]) ? -1*DIM[tmonth]+1 : ovrflw--;
   if (ovrflw) tmonth += (tmonth == 12) ? -11 : ovrflw--;
   tyear=tyear+ovrflw;

   *outYear  = tyear;
   *outMonth = tmonth;
   *outDay   = tday;
   *outHour  = thour;
   *outMin   = tmin;
   *outSec   = tsec;

 }

int gmt2month(int year, int gmt_day)
 {
   static int DIM[13]={0,31,28,31,30,31,30,31,31,30,31,30,31};/*Days in Month*/
   int month, day;

   if (year%4==0) DIM[2]=29; /* Check for Leap Year */
   day = gmt_day;
   month = 1;
   while(day > 0) { day -= DIM[month+1]; month++; }
   return(month-1);
 }

int gmt2day(int year, int gmt_day)
 {
   static int DIM[13]={0,31,28,31,30,31,30,31,31,30,31,30,31}; /*Days in Month*/
   int month, day;

   if (year%4==0) DIM[2]=29; /* Check for Leap Year */
   day = gmt_day;
   month = 1;
   while(day > 0) { day -= DIM[month+1]; month++; }
   day += DIM[month];
   return(day-1);
 }

void fill_dssr(meta_parameters *meta, struct DDR *ddr, double centerTime,
	  struct dataset_sum_rec *dssr)
 {
    if (meta->geo->lookDir == 'R') { dssr->clock_ang = 90.0; }
    else { dssr->clock_ang = -90.0; }

    dssr->wave_length = meta->geo->wavelen;

    strcpy(dssr->time_dir_pix,"INCREASE");
    if (meta->geo->azPixTime < 0.0) strcpy(dssr->time_dir_lin,"DECREASE");
    else strcpy(dssr->time_dir_lin,"INCREASE");
    strcpy(dssr->line_cont,"RANGE");

    dssr->alt_dopcen[0] = meta->geo->dopAz[0];
    dssr->alt_dopcen[1] = meta->geo->dopAz[1];
    dssr->alt_dopcen[2] = meta->geo->dopAz[2];
    dssr->crt_dopcen[0] = meta->geo->dopRange[0];
    dssr->crt_dopcen[1] = meta->geo->dopRange[1];
    dssr->crt_dopcen[2] = meta->geo->dopRange[2];

    sprintf(dssr->revolution,"%i",meta->info->orbit);
    strcpy(dssr->sat_bintim,meta->info->satBinTime);
    strcpy(dssr->sat_clktim,meta->info->satClkTime);

 }

void fill_ppdr(meta_parameters *meta, struct DDR *ddr,
	 stateVector *sVec, stateVector *cVec, stateVector *eVec,
	 struct pos_data_rec *ppdr)
 {
    int hour, min;
    double sec;

    void ctok(double sc_vec[6], double kepler[6]);

    ppdr->ndata = meta->stVec->num;
    ppdr->year = meta->stVec->year;
    ppdr->month = gmt2month(meta->stVec->year, meta->stVec->julDay);
    ppdr->day = gmt2day(meta->stVec->year, meta->stVec->julDay);
    ppdr->gmt_day = meta->stVec->julDay;
    ppdr->gmt_sec = meta->stVec->second;
    ppdr->data_int = fabs(imageTimeLen/(meta->stVec->num-1));

    hour=(int)(ppdr->gmt_sec/3600);
    min=((int)ppdr->gmt_sec-3600*hour)/60;
    sec=ppdr->gmt_sec-3600*hour-60*min;

    ppdr->hr_angle = utc2gha(ppdr->year,ppdr->gmt_day,hour,min,sec);

    if (meta->geo->azPixTime<0.0)
     {
       stateVector tVec;
    
       tVec = *sVec;
       *sVec = *eVec;
       *eVec = tVec;
     }

    fixed2gei(sVec,ppdr->hr_angle);
    fixed2gei(cVec,ppdr->hr_angle+ppdr->data_int*WE);
    fixed2gei(eVec,ppdr->hr_angle+2.0*ppdr->data_int*WE);

    ppdr->pos_vec[0][0] = sVec->pos.x/1000.0;
    ppdr->pos_vec[0][1] = sVec->pos.y/1000.0;
    ppdr->pos_vec[0][2] = sVec->pos.z/1000.0;
    ppdr->pos_vec[0][3] = sVec->vel.x;
    ppdr->pos_vec[0][4] = sVec->vel.y;
    ppdr->pos_vec[0][5] = sVec->vel.z;
    ppdr->pos_vec[1][0] = cVec->pos.x/1000.0;
    ppdr->pos_vec[1][1] = cVec->pos.y/1000.0;
    ppdr->pos_vec[1][2] = cVec->pos.z/1000.0;
    ppdr->pos_vec[1][3] = cVec->vel.x;
    ppdr->pos_vec[1][4] = cVec->vel.y;
    ppdr->pos_vec[1][5] = cVec->vel.z;
    ppdr->pos_vec[2][0] = eVec->pos.x/1000.0;
    ppdr->pos_vec[2][1] = eVec->pos.y/1000.0;
    ppdr->pos_vec[2][2] = eVec->pos.z/1000.0;
    ppdr->pos_vec[2][3] = eVec->vel.x;
    ppdr->pos_vec[2][4] = eVec->vel.y;
    ppdr->pos_vec[2][5] = eVec->vel.z;

    ctok(ppdr->pos_vec[0],ppdr->orbit_ele);
 }
	
void fill_facdr(meta_parameters *meta, struct DDR *ddr, 
	stateVector *cVec, double centerTime, struct VFDRECV *facdr)
 {
   facdr->dpplrfrq = meta->geo->dopRange[0];
   facdr->dpplrslp = meta->geo->dopRange[1];
   facdr->dpplrqdr = meta->geo->dopRange[2];

   facdr->scxpos = cVec->pos.x / 1000.0;
   facdr->scypos = cVec->pos.y / 1000.0;
   facdr->sczpos = cVec->pos.z / 1000.0;
   facdr->scxvel = cVec->vel.x;
   facdr->scyvel = cVec->vel.y;
   facdr->sczvel = cVec->vel.z;


   if (meta->geo->deskew == 1) strcpy(facdr->deskewf,"YES");
   else strcpy(facdr->deskewf,"NOT");

   facdr->sltrngfp = (meta->geo->slantFirst + meta->geo->slantShift) / 1000.0;
   facdr->eradcntr = meta->ifm->er / 1000.0;
   facdr->swathvel = fabs(facdr->swazim/imageTimeLen)*1000.0;

   if (meta->geo->type == 'S') {
	strcpy(facdr->grndslnt,"SLANT");
	facdr->sltrnglp = (ddr->ns*meta->geo->xPix)/1000.0 + facdr->sltrngfp;
   } else {
	strcpy(facdr->grndslnt,"GROUND");
	facdr->sltrnglp = getSRfromGR(*cVec,facdr->sltrngfp,facdr->eradcntr,
                (ddr->ns*ddr->pdist_x)/1000.0);
   }

  facdr->biterrrt = meta->info->bitErrorRate;

/*
  double tmpDop;
  double tmpVel;

  tmpVel = vecMagnitude(cVec->vel);
  tmpDop = facdr->dpplrfrq 
	   + facdr->dpplrslp*(ddr->ns/2.0) 
	   + facdr->dpplrqdr*SQR(ddr->ns/2.0);

  facdr->squintan = asin((meta->geo->wavelen*tmpDop)/(2.0*facdr->swathvel));
  facdr->squintan = asin((meta->geo->wavelen*tmpDop)/(2.0*tmpVel));
*/
 }
