/* SccsId[]= @(#)ssp_get_rqst.c	2.41 3/24/98 */
static char sccsid_ssp_get_rqst[]= "@(#)PPssp_get_rqst.c:2.41";

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "odl.h"
#include "error.h"
#include "ssp_req_odl.h"
FILE *fp;

int    load_rqst_odl (file_odl,
                  proc_id,product_id,prod_type,comp_flag,
                  gha_corr_time,gha_corr_angle,instr_mode,sv_type,
                  sv_coord_sys,sv_time,sv_x_pos,sv_y_pos,
                  sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
                  frame_mode,output_format,projection,pixel_spacing,
                  ref_lat,ref_lon,utm_zone,ter_correct,
                  proc_gain,avg_terrain,echo_file,aux_file,
                  rep_file,eph_file,cali_file,ceos_leader,image_file,
                  scan_results_file,burst_loc_file,pmf_file,
                  msg_type,dest,source,time,number_records,
                  platform,sensor,rev,sequence,activity_id,
                  start_address,end_address,start_time,end_time,
                  record_id,station_id,sv_satellite,sv_rev,
                  sv_number_vec,frame_id,subframe_id,sitename,lamb_lat_n,
                  lamb_lat_s,media_id,media_type,media_location)
/*******************************/
char *file_odl;
int *proc_id;
char *product_id;
char *prod_type;
char *comp_flag;
char *gha_corr_time;
double *gha_corr_angle;
char *instr_mode;
char *sv_type;
char *sv_coord_sys;
char *sv_time;
double *sv_x_pos;
double *sv_y_pos;
double *sv_z_pos;
double *sv_x_vel; 
double *sv_y_vel;
double *sv_z_vel;
char *frame_mode;
char *output_format;
char *projection;
double *pixel_spacing;
double *ref_lat;
double *ref_lon;
int *utm_zone;
char *ter_correct;
int *proc_gain;
double *avg_terrain;
char *echo_file;
char *aux_file;
char *rep_file;
char *eph_file;
char *cali_file;
char *ceos_leader;
char *image_file;
char *scan_results_file;
char *burst_loc_file;
char *pmf_file;
char *msg_type;
char *dest;
char *source;
char *time;
int *number_records;
char *platform;
char *sensor;
int  *rev;
int  *sequence;
char *activity_id;
int  *start_address;
int  *end_address;
char *start_time;
char *end_time;
char *record_id;
char *station_id;
char *sv_satellite;
int  *sv_rev;
int  *sv_number_vec;
int  *frame_id;
int  *subframe_id;
char *sitename;
double *lamb_lat_n;
double *lamb_lat_s;
char *media_id;
char *media_type;
char *media_location;
/*******************************/
{
int   	d_proc_id;
char   	d_product_id[60];
char 	d_prod_type[60];
char    d_comp_flag[60];
char 	d_gha_corr_time[60];
double 	d_gha_corr_angle;
char 	d_instr_mode[60];
char 	d_sv_type[60];
char 	d_sv_coord_sys[60];
char 	d_sv_time[60];
double 	d_sv_x_pos;
double 	d_sv_y_pos;
double 	d_sv_z_pos;
double 	d_sv_x_vel;
double 	d_sv_y_vel;
double 	d_sv_z_vel;
char 	d_frame_mode[60];
char 	d_output_format[60];
char 	d_projection[60];
double 	d_pixel_spacing;
double 	d_ref_lat;
double 	d_ref_lon;
int 	d_utm_zone;
char 	d_ter_correct[60];
int     d_proc_gain;
double  d_avg_terrain;
char 	d_echo_file[256];
char 	d_aux_file[256];
char 	d_rep_file[256];
char 	d_eph_file[256];
char 	d_cali_file[256];
char 	d_ceos_leader[256];
char    d_image_file[256];
char    d_scan_results_file[256];
char 	d_burst_loc_file[256];
char    d_pmf_file[256];

char    d_msg_type[60];
char    d_dest[60];
char    d_source[60];
char    d_time[60];
int     d_number_records;
char    d_platform[60];
char    d_sensor[60];
int     d_rev;
int     d_sequence;
char    d_activity_id[60];
int     d_start_address;
int     d_end_address;
char    d_start_time[60];
char    d_end_time[60];
char    d_record_id[60];
char    d_station_id[60];
char    d_sv_satellite[60];
int     d_sv_rev;
int     d_sv_number_vec;
int     d_frame_id;
int     d_subframe_id;
char    d_sitename[60];
double  d_lamb_lat_n;
double  d_lamb_lat_s;
char    d_media_id[60];
char    d_media_type[60];
char    d_media_location[60];



/*******************************/
  char error_msg[60];
  ODL odl;
    struct stat stbuf;
    char *buf;
    char *sss;
    int fd;
    char filename[256];
    char buff[500];

str_f_to_c(filename,file_odl,256);


    if ((fd = open(filename, 0)) == -1) {
      sprintf(buff,"cannot open %s during parsing %s",filename,filename);
      printclog(3,buff);
      printerr(filename);
      return (ierr_2);
    }
    if (fstat(fd, &stbuf) == -1) {
      sprintf(buff,"cannot open %s during parsing %s",filename,filename);
      printclog(3,buff);
      printerr(filename);
        return (ierr_2);
    }

    if ((buf = (char*) malloc(stbuf.st_size)) == NULL) {
      sprintf(buff,"cannot malloc during parsing %s",filename);
      printclog(3,buff);
        return (ierr_5);
    }
    if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      sprintf(buff,"cannot read during parsing %s",filename);
      printclog(3,buff);
      printerr(filename);
	return (ierr_4);
    }
    close (fd);


    if ((odl = StrToODL(buf, stbuf.st_size)) == NULL) {
      sprintf(buff,"cannot parse through %s",filename);
      printclog(3,buff);
	return (ierr_11);
    }

  free(buf);

/*  sss= (char *)malloc(sizeof(char)*1000); */
  sss=  ODLToStr(odl,NULL);



if (    ssp_get_rqst (odl, error_msg,
                  &d_proc_id,&d_product_id,d_prod_type,d_comp_flag,
                  d_gha_corr_time,&d_gha_corr_angle,d_instr_mode,d_sv_type,
                  d_sv_coord_sys,d_sv_time,&d_sv_x_pos,&d_sv_y_pos,
                  &d_sv_z_pos,&d_sv_x_vel,&d_sv_y_vel,&d_sv_z_vel,
                  d_frame_mode,d_output_format,d_projection,&d_pixel_spacing,
                  &d_ref_lat,&d_ref_lon,&d_utm_zone,d_ter_correct,
                  &d_proc_gain,&d_avg_terrain,d_echo_file,d_aux_file,
                  d_rep_file,d_eph_file,d_cali_file,d_ceos_leader,d_image_file,
                  d_scan_results_file,d_burst_loc_file,d_pmf_file,
                  d_msg_type,d_dest,d_source,d_time,&d_number_records,
                  d_platform,d_sensor,&d_rev,&d_sequence,d_activity_id,
                  &d_start_address,&d_end_address,d_start_time,d_end_time,
                  d_record_id,d_station_id,d_sv_satellite,&d_sv_rev,
                  &d_sv_number_vec,&d_frame_id,&d_subframe_id,d_sitename,&d_lamb_lat_n,
                  &d_lamb_lat_s,d_media_id,d_media_type,d_media_location)== -1)

   {
    printf("%s \n",error_msg);
    sprintf(buff,"%s parsing through %s",error_msg,filename);
    printclog(3,buff);
    return(ierr_12);
   }


*proc_id	= d_proc_id;
*gha_corr_angle	= d_gha_corr_angle;
*sv_x_pos	= d_sv_x_pos;
*sv_y_pos	= d_sv_y_pos;
*sv_z_pos	= d_sv_z_pos;
*sv_x_vel	= d_sv_x_vel;
*sv_y_vel	= d_sv_y_vel;
*sv_z_vel	= d_sv_z_vel;
*pixel_spacing  = d_pixel_spacing;
*ref_lat	= d_ref_lat;
*ref_lon	= d_ref_lon;
*utm_zone	= d_utm_zone;
*proc_gain	= d_proc_gain;
*avg_terrain	= d_avg_terrain;
*number_records = d_number_records;
*rev            = d_rev;
*sequence       = d_sequence;
*start_address  = d_start_address;
*end_address    = d_end_address;
*sv_rev         = d_sv_rev;
*sv_number_vec  = d_sv_number_vec;
*frame_id       = d_frame_id;
*subframe_id    = d_subframe_id;
*lamb_lat_n     = d_lamb_lat_n;
*lamb_lat_s      = d_lamb_lat_s;

str_c_to_f(product_id,d_product_id,60);
str_c_to_f(prod_type,d_prod_type,60);
str_c_to_f(comp_flag,d_comp_flag,60);
str_c_to_f(gha_corr_time,d_gha_corr_time,60);
str_c_to_f(instr_mode,d_instr_mode,60);
str_c_to_f(sv_type,d_sv_type,60);
str_c_to_f(sv_coord_sys,d_sv_coord_sys,60);
str_c_to_f(sv_time,d_sv_time,60);
str_c_to_f(frame_mode,d_frame_mode,60);
str_c_to_f(output_format,d_output_format,60);
str_c_to_f(projection,d_projection,60);
str_c_to_f(ter_correct,d_ter_correct,60);
str_c_to_f(echo_file   ,  d_echo_file,256);
str_c_to_f(aux_file    ,  d_aux_file,256);
str_c_to_f(rep_file    ,  d_rep_file,256);
str_c_to_f(eph_file    ,  d_eph_file,256);
str_c_to_f(cali_file   ,  d_cali_file,256);
str_c_to_f(ceos_leader ,  d_ceos_leader,256);
str_c_to_f(image_file  ,  d_image_file,256);
str_c_to_f(scan_results_file  ,  d_scan_results_file,256);
str_c_to_f(burst_loc_file  ,  d_burst_loc_file,256);
str_c_to_f(pmf_file  ,  d_pmf_file,256);
str_c_to_f(msg_type  ,  d_msg_type,60);
str_c_to_f(dest      ,  d_dest,60);
str_c_to_f(source    ,  d_source,60);
str_c_to_f(time      ,  d_time,60);
str_c_to_f(platform  ,  d_platform,60);
str_c_to_f(sensor    ,  d_sensor,60);
str_c_to_f(activity_id, d_activity_id,60);
str_c_to_f(start_time,  d_start_time,60);
str_c_to_f(end_time  ,  d_end_time,60);
str_c_to_f(record_id,   d_record_id,60);
str_c_to_f(station_id,  d_station_id,60);
str_c_to_f(sv_satellite,d_sv_satellite,60);
str_c_to_f(sitename,    d_sitename,60);
str_c_to_f(media_id,    d_media_id,60);
str_c_to_f(media_type,    d_media_type,60);
str_c_to_f(media_location,    d_media_location,60);













 




   return(iok);



}



int    ssp_get_rqst (msg, error_msg,
                  proc_id,product_id,prod_type,comp_flag,
                  gha_corr_time,gha_corr_angle,instr_mode,sv_type,
                  sv_coord_sys,sv_time,sv_x_pos,sv_y_pos,
                  sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
                  frame_mode,output_format,projection,pixel_spacing,
                  ref_lat,ref_lon,utm_zone,ter_correct,
                  proc_gain,avg_terrain,echo_file,aux_file,
                  rep_file,eph_file,cali_file,ceos_leader,image_file,
                  scan_results_file,burst_loc_file,pmf_file,
                  msg_type,dest,source,time,number_records,
                  platform,sensor,rev,sequence,activity_id,
                  start_address,end_address,start_time,end_time,
                  record_id,station_id,sv_satellite,sv_rev,
                  sv_number_vec,frame_id,subframe_id,sitename,lamb_lat_n,
                  lamb_lat_s,media_id,media_type,media_location)


/**********************************************************/
ODL msg;
char *error_msg;
/**********************************************************/
char *media_id;
char *media_type;
char *media_location;
int *proc_id;
char *product_id;
char *prod_type;
char *comp_flag;
char *gha_corr_time;
double *gha_corr_angle;
char *instr_mode;
char *sv_type;
char *sv_coord_sys;
char *sv_time;
double *sv_x_pos;
double *sv_y_pos;
double *sv_z_pos;
double *sv_x_vel; 
double *sv_y_vel;
double *sv_z_vel;
char *frame_mode;
char *output_format;
char *projection;
double *pixel_spacing;
double *ref_lat;
double *ref_lon;
int *utm_zone;
char *ter_correct;
int *proc_gain;
double *avg_terrain;
char *echo_file;
char *aux_file;
char *rep_file;
char *eph_file;
char *cali_file;
char *ceos_leader;
char *image_file;
char *scan_results_file;
char *burst_loc_file;
char *pmf_file;
char *msg_type;
char *dest;
char *source;
char *time;
int *number_records;
char *platform;
char *sensor;
int  *rev;
int  *sequence;
char *activity_id;
int  *start_address;
int  *end_address;
char *start_time;
char *end_time;
char *record_id;
char *station_id;
char *sv_satellite;
int  *sv_rev;
int  *sv_number_vec;
int  *frame_id;
int  *subframe_id;
char *sitename;
double *lamb_lat_n;
double *lamb_lat_s;






{
  
  int ierr;
  char dummy_str[60];

/***********************************************/
/***********************************************/
/*  strcpy(msg_type , ODLGetString(msg, MSG_TYPE, &ierr));*/
    strcpy(dummy_str , ODLGetString(msg, MSG_TYPE, &ierr));  
  if (ierr != 0) {
    strcpy(error_msg , "Can't get MSG_TYPE");
    return(-1);
  }
  
  strcpy(dest , ODLGetString(msg, DESTINATION, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get DESTINATION");
    return(-1);
  }

  strcpy(source , ODLGetString(msg, SOURCE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SOURCE");
    return(-1);
  }

  strcpy(time , ODLGetTime(msg, TIME, &ierr));
  if (ierr != 0) { 
    strcpy(error_msg , "Can't get TIME");
    return(-1);
  }

  *number_records = ODLGetInt(msg, NUMBER_RECORDS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get NUMBER_OF_RECORDS");
    return(-1);
  }

  *proc_id = ODLGetInt(msg, JOB_ID, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get JOB_ID");
    return(-1);
  }

  strcpy(product_id,ODLGetString(msg,PRODUCT_ID, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get PRODUCT_ID");
    return(-1);
  }

  strcpy(platform , ODLGetString(msg,PLATFORM, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get PLATFORM");
    return(-1);
  }

  strcpy(sensor , ODLGetString(msg, SENSOR, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SENSOR");
    return(-1);
  }
  
  *rev = ODLGetInt(msg, REV, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get REV");
    return(-1);
  }

  *sequence = ODLGetInt(msg, SEQ, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SEQ");
    return(-1);
  }

  strcpy(activity_id , ODLGetString(msg, ACTIVITY, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get ACTIVITY");
    return(-1);
  }
  
  *start_address = ODLGetInt(msg, START_ADDRESS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get START_address");
    return(-1);
  }
  
  *end_address = ODLGetInt(msg, END_ADDRESS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get END_ADDRESS");
    return(-1);
  }
  
  strcpy(start_time , ODLGetTime(msg, START_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get START_TIME");
    return(-1);
  }
  
  strcpy(end_time , ODLGetTime(msg,END_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get END_TIME");
    return(-1);
  }

  strcpy(record_id , ODLGetString(msg, RECORDID, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get RECORD_ID");
    return(-1);
  }

  strcpy(station_id , ODLGetString(msg, STATION_ID, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get STATION_ID");
    return(-1);
  }

  strcpy(gha_corr_time , ODLGetTime(msg, GHA_CORR_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get GHA_CORR_TIME");
    return(-1);
  }

  *gha_corr_angle = ODLGetDouble(msg, GHA_CORR_ANGLE, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get GHA_CORR_ANGLE");
    return(-1);
  }

  strcpy(instr_mode , ODLGetString(msg, INSTRUMENT_MODE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get INSTRUMENT_MODE");
    return(-1);
  }

  strcpy(sv_satellite , 
    ODLGetString(msg,
                 SV_SATELLITE,
		 &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_SATELLITE");
    return(-1);
  }

  strcpy(sv_type , ODLGetString(msg, SV_TYPE_PRECISION, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_TYPE");
    return(-1);
  }

  strcpy(sv_coord_sys ,
    ODLGetString(msg,
		SV_COORD_SYS 
		 , &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_COORD_SYS");
    return(-1);
  }

  *sv_rev = 
    ODLGetInt(msg,
              SV_REV
	      ,&ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_REV");
    return(-1);
  }

/* *sv_number_vec = ODLGetInt(msg, SV_NUMBER_VEC, &ierr);   */
/*  if (ierr != 0) {                                        */
/*    strcpy(error_msg , "Can't get SV_NUMBER_OF_VECTOR");  */
/*    return(-1);                                           */
/*  }                                                       */
  
  strcpy(sv_time , ODLGetTime(msg, SV_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get State_Vector_Date");
    return(-1);
  }

  *sv_x_pos = ODLGetDouble(msg, SV_X_POS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_X_POS");
    return(-1);
  }

  *sv_y_pos = ODLGetDouble(msg, SV_Y_POS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_Y_POS");
    return(-1);
  }

  *sv_z_pos = ODLGetDouble(msg, SV_Z_POS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_Z_POS");
    return(-1);
  }

  *sv_x_vel = ODLGetDouble(msg, SV_X_VEL, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_X_VEL");
    return(-1);
  }

  *sv_y_vel = ODLGetDouble(msg, SV_Y_VEL, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_Y_VEL");
    return(-1);
    }

  *sv_z_vel = ODLGetDouble(msg, SV_Z_VEL, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SV_Z_VEL");
    return(-1);
  }

  strcpy(prod_type , ODLGetString(msg, PROD_TYPE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get PROD_TYPE");
    return(-1);
  }

  strcpy(comp_flag , ODLGetString(msg, COMP_FLAG, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get COMP_FLAG");
    return(-1);
  }

  *pixel_spacing = ODLGetDouble(msg, PIXEL_SPACING, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get PIXEL_SPACING");
    return(-1);
  }

  *frame_id = ODLGetInt(msg, FRAME_ID, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get FRAME_ID");
    return(-1);
  }

  *subframe_id = ODLGetInt(msg, SUBFRAME_ID, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SUBFRAME_ID");
    return(-1);
  }

  strcpy(frame_mode , ODLGetString(msg, FRAME_MODE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get FRAME_MODE");
    return(-1);
  }

  strcpy(output_format , ODLGetString(msg, OUTPUT_FORMAT, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get OUTPUT_FORMAT");
    return(-1);
  }

  strcpy(sitename , ODLGetString(msg, SITENAME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get SITENAME");
    return(-1);
  }

  strcpy(projection , ODLGetString(msg, PROJECTION, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get PROJECTION");
    return(-1);
  }

  *proc_gain = ODLGetInt(msg, PROC_GAIN, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get PROC_GAIN");
    return(-1);
  }

  *avg_terrain= ODLGetDouble(msg, AVG_TERRAIN, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get AVG_TERRAIN");
    return(-1);
  }
 
  *ref_lat = ODLGetDouble(msg, REF_LAT, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get REFERENCE_LAT");
    return(-1);
  }

  *ref_lon = ODLGetDouble(msg, REF_LON, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get REFERENCE_LON");
    return(-1);
  }

  *utm_zone = ODLGetInt(msg, UTM_ZONE, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get UTM_ZONE");
    return(-1);
  }

  strcpy(ter_correct , ODLGetString(msg, TER_CORRECT, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get TERRAIN_CORRECTION");
    return(-1);
  }

  *lamb_lat_n = ODLGetDouble(msg, LAMB_LAT_N, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't getLAMB_LAT_N");
    return(-1);
  }

  *lamb_lat_s = ODLGetDouble(msg, LAMB_LAT_S, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't getLAMB_LAT_S");
    return(-1);
  }


  strcpy(cali_file , ODLGetString(msg, CALI_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get CALIBRATION_FILE");
    return(-1);
  }

  strcpy(scan_results_file , ODLGetString(msg, FRAME_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get FRAME_FILE");
    return(-1);
  }

  strcpy(image_file , ODLGetString(msg, CEOS_DATA, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get CEOS_DATA_FILE");
    return(-1);
  }

  strcpy(ceos_leader , ODLGetString(msg, CEOS_LEADER, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get CEOS_LEADER_FILE");
    return(-1);
  }

  strcpy(pmf_file , ODLGetString(msg, PMF_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get PMF_FILE_FILE");
    return(-1);
  }

  strcpy(echo_file,ODLGetString(msg, ECHO_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get ECHO_FILE");
    return(-1);
  }

  strcpy(aux_file , ODLGetString(msg, AUX_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get AUXILIARY_FILE");
    return(-1);
  }

  strcpy(eph_file , ODLGetString(msg, EPH_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get EPHEMERIS_FILE");
    return(-1);
  }

  strcpy(rep_file , ODLGetString(msg, REP_FILE, &ierr));
  if (ierr !=0) {
    strcpy(error_msg , "Can't get REPLICA_FILE");
    return(-1);
  }

  strcpy(burst_loc_file , ODLGetString(msg, BURST_LOC_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get BURST_LOC_FILE");
    return(-1);
  }

  strcpy(media_id , ODLGetString(msg, MEDIA_ID, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get MEDIA_ID");
    return(-1);
  }
  strcpy(media_type , ODLGetString(msg, MEDIA_TYPE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get MEDIA_TYPE");
    return(-1);
  }
  strcpy(media_location , ODLGetString(msg, MEDIA_LOCATION, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "Can't get MEDIA_LOCATION");
    return(-1);
  }

/***********************************************/
/***********************************************/


  return(0);
      
}

