/* SccsId[]= @(#)write_info.c	2.41 3/24/98 */
static char sccsid_write_info[]= "@(#)PPwrite_info.c:2.41";

#include "pp.h"

void write_info(char *file_name,
		int record_length,int number_of_records,double ratio,
		int clutter_flag,int auto_focus_flag,double delta_t1,
		double delta_t2,double image_mean_earth_radius,
		double nadir_mean_earth_radius)
{
  FILE *fptr;
  
  float zero = 0.0;
  float one = 1.0;
  float nine = -999999.0;
  double r_center, rg_center, vsg, platform_heading, fd_crn1;
  char gmt[24], gmt_digit_only[24], start_gmt[24], end_gmt[24];

  double r_far,fr_far,t_offset,t_spacing,t_frame,t_center,
    xs1,ys1,zs1,vxs1,vys1,vzs1,xs_bf,ys_bf,zs_bf,vxs_bf,vys_bf,vzs_bf,
    ground_range,slant_range,px,py,pz,xt,yt,zt,vxt,vyt,vzt,look_angle_local,
    yaw,thi_inc,incident_angle,rng_res,slant_range_last_pxl,
    slant_range_mid_pxl;

  double x_begin, y_begin, z_begin, vx_begin, vy_begin, vz_begin,
    x_end, y_end, z_end, vx_end, vy_end, vz_end,
    x_middle, y_middle, z_middle, vx_middle, vy_middle, vz_middle,
    angle1, angle2, len, track_angle, delta_t_begin, delta_t_end, 
    delta_t_middle;

  int start_sample, start_line, i, j, inc;
  double *tmp_buffer, *spectrum, average;

  /*********** calculate nx_spacing ***********/
  r_center = r0_base + dr*((int)(ns-plen*fs)/2);
  sr2gr(&r_center,&rg_center,
       &rng_in,&az_in,&nadd,&n_max,&pi,&c,&pxl_unit,
       &ers,&jers,&rsat_s12,&rsat_s37,&rsat_ws,&rsat_fr,&rsat_eh,&rsat_el,
       buff_in,array,buff_in_rd,
       &mode,
       &raw_mode,&i_dop_mode,&nband,&nlines,&line_offset,
       &ns,&nr_header,&ns_header,&i_squint,&i_pt_sim,&i_data_type,&nlook,
       &nfr,&nfx,&icomplex,&i_preproc,&i_iqqi,&nx_offset,&i_dummy,
       &lambda,&fc,&bw,&dmu,&fs,&plen,&k_chirp,&dr,
       &prf,&prf_in,&pbw,&az_ang,&az_res,&df,
       &t_1st_echo,&t_eph,&t_gha,&gha,&xs,&ys,&zs,&rsc,&vxs,&vys,&vzs,&vs,
       &re_mean,&h,
       &rs_ref,&r0_base,&fd0_base,&fr0_base,&fd0,&fr0,&fdd,&frd,
       &fd_const,&r_ref,&r_1st,
       &pxl_spacing,&nx_spacing,
       &t_start_frm,&t_end_frm,
       &lat1,&lon1,&rs_crn1,&rg_crn1,&lookang1,
       &lat2,&lon2,&rs_crn2,&rg_crn2,&lookang2,
       &lat3,&lon3,&rs_crn3,&rg_crn3,&lookang3,
       &lat4,&lon4,&rs_crn4,&rg_crn4,&lookang4,
       &lat5,&lon5,&rs_crn5,&rg_crn5,&lookang5,
       raw_file,image_file);


  /*********** calculate nx_spacing ***********/
  vsg = vs * re_mean*cos(rg_center/re_mean)/rsc;
  nx_spacing = vsg/prf;


  /*********** convert image center time to GMT format ***********/
  sec2gmt(t_1st_echo+(delta_t2+delta_t1)/2, gmt);  
  seconds_to_time(t_1st_echo+(delta_t2+delta_t1)/2,gmt_digit_only);
  sec2gmt(t_start_frm, start_gmt);
  sec2gmt(t_end_frm, end_gmt);  


  /*********** calculate platform heading ***********/
  line_hdng(&lat1,&lat3,&lon1,&lon3,&platform_heading);



  /*********** calculate incident angle and look angle ***********/
  /* Propagate to the center of image */
  t_spacing = 2.e-4;
  t_center = (t_start_frm + t_end_frm)/2;  
  delta_t_middle = t_center - t_1st_echo;
  propa_simp(&xs,&ys,&zs,&vxs,&vys,&vzs,&delta_t_middle,&t_spacing,
	     &xs1,&ys1,&zs1,&vxs1,&vys1,&vzs1);
  /* convert to EBF */
  eme_to_ebf(&gha,&t_gha,&t_center,&xs1,&ys1,&zs1,&xs_bf,&ys_bf,&zs_bf);
  eme_to_ebf(&gha,&t_gha,&t_center,&vxs1,&vys1,&vzs1,&vxs_bf,&vys_bf,&vzs_bf);
  if(icomplex == 0) {
    /* slant range of 1st pixel */
    sr2gr(&r0_base,&ground_range,
	  &rng_in,&az_in,&nadd,&n_max,&pi,&c,&pxl_unit,
	  &ers,&jers,&rsat_s12,&rsat_s37,&rsat_ws,&rsat_fr,&rsat_eh,&rsat_el,
	  buff_in,array,buff_in_rd,
	  &mode,
	  &raw_mode,&i_dop_mode,&nband,&nlines,&line_offset,
	  &ns,&nr_header,&ns_header,&i_squint,&i_pt_sim,&i_data_type,&nlook,
	  &nfr,&nfx,&icomplex,&i_preproc,&i_iqqi,&nx_offset,&i_dummy,
	  &lambda,&fc,&bw,&dmu,&fs,&plen,&k_chirp,&dr,
	  &prf,&prf_in,&pbw,&az_ang,&az_res,&df,
	  &t_1st_echo,&t_eph,&t_gha,&gha,&xs,&ys,&zs,&rsc,&vxs,&vys,&vzs,&vs,
	  &re_mean,&h,
	  &rs_ref,&r0_base,&fd0_base,&fr0_base,&fd0,&fr0,&fdd,&frd,
	  &fd_const,&r_ref,&r_1st,
	  &pxl_spacing,&nx_spacing,
	  &t_start_frm,&t_end_frm,
	  &lat1,&lon1,&rs_crn1,&rg_crn1,&lookang1,
	  &lat2,&lon2,&rs_crn2,&rg_crn2,&lookang2,
	  &lat3,&lon3,&rs_crn3,&rg_crn3,&lookang3,
	  &lat4,&lon4,&rs_crn4,&rg_crn4,&lookang4,
	  &lat5,&lon5,&rs_crn5,&rg_crn5,&lookang5,
	  raw_file,image_file);
    ground_range += nfr*pxl_spacing/2.0;
    gr2sr(&ground_range, &slant_range,
	  &rng_in,&az_in,&nadd,&n_max,&pi,&c,&pxl_unit,
	  &ers,&jers,&rsat_s12,&rsat_s37,&rsat_ws,&rsat_fr,&rsat_eh,&rsat_el,
	  buff_in,array,buff_in_rd,
	  &mode,
	  &raw_mode,&i_dop_mode,&nband,&nlines,&line_offset,
	  &ns,&nr_header,&ns_header,&i_squint,&i_pt_sim,&i_data_type,&nlook,
	  &nfr,&nfx,&icomplex,&i_preproc,&i_iqqi,&nx_offset,&i_dummy,
	  &lambda,&fc,&bw,&dmu,&fs,&plen,&k_chirp,&dr,
	  &prf,&prf_in,&pbw,&az_ang,&az_res,&df,
	  &t_1st_echo,&t_eph,&t_gha,&gha,&xs,&ys,&zs,&rsc,&vxs,&vys,&vzs,&vs,
	  &re_mean,&h,
	  &rs_ref,&r0_base,&fd0_base,&fr0_base,&fd0,&fr0,&fdd,&frd,
	  &fd_const,&r_ref,&r_1st,
	  &pxl_spacing,&nx_spacing,
	  &t_start_frm,&t_end_frm,
	  &lat1,&lon1,&rs_crn1,&rg_crn1,&lookang1,
	  &lat2,&lon2,&rs_crn2,&rg_crn2,&lookang2,
	  &lat3,&lon3,&rs_crn3,&rg_crn3,&lookang3,
	  &lat4,&lon4,&rs_crn4,&rg_crn4,&lookang4,
	  &lat5,&lon5,&rs_crn5,&rg_crn5,&lookang5,
	  raw_file,image_file);
  } else {
    slant_range = r0_base + nfr*dr/2.0;
  }	
  slant_range_mid_pxl = slant_range;
  fd_crn1 = fd0_base + fdd* (slant_range - r0_base);
  /* initial guess for look_angle */
  look_angle_local = look_angle*180.0/pi;
  /* Calculate the look angle */
  gttarg1(&slant_range, &fd_crn1, &lambda, 
	  &xs_bf,&ys_bf,&zs_bf, &vxs_bf,&vys_bf,&vzs_bf, 
	  &h, &px, &py, &pz, &xt, &yt, &zt, &vxt, &vyt, &vzt, 
	  &look_angle_local, &yaw);
  /* Calculate the incidence angle in degrees */
  inc_ang(&look_angle_local,&xs_bf,&ys_bf,&zs_bf,&xt,&yt,&zt,&thi_inc);
  incident_angle = thi_inc;


  /******* satellite track angle and state vectors at different stages *******/
  delta_t_begin = t_start_frm - t_1st_echo;
  propa_simp(&xs,&ys,&zs,&vxs,&vys,&vzs,&delta_t_begin,&t_spacing,
	     &x_begin,&y_begin,&z_begin,&vx_begin,&vy_begin,&vz_begin);
  delta_t_end = t_end_frm - t_1st_echo;
  propa_simp(&xs,&ys,&zs,&vxs,&vys,&vzs,&delta_t_end,&t_spacing,
	     &x_end,&y_end,&z_end,&vx_end,&vy_end,&vz_end);
  delta_t_middle = t_center - t_1st_echo;
  propa_simp(&xs,&ys,&zs,&vxs,&vys,&vzs,&delta_t_middle,&t_spacing,
	     &x_middle,&y_middle,&z_middle,&vx_middle,&vy_middle,&vz_middle);
  angle1 = acos(sqrt((x_begin*x_begin+y_begin*y_begin)/
		     (x_begin*x_begin+y_begin*y_begin+z_begin*z_begin)));
  angle2 = acos(sqrt((x_end*x_end+y_end*y_end)/
		     (x_end*x_end+y_end*y_end+z_end*z_end)));
  len = nfx*pxl_spacing;
  if(z_begin > 0.0 && z_end < 0.0) {
    track_angle = (180.0/pi)*asin(nadir_mean_earth_radius*fabs(angle1+angle2)/len);   
  } else {
    track_angle = (180.0/pi)*asin(nadir_mean_earth_radius*fabs(angle1-angle2)/len);
  }

  /* Acording to Ming's cheap fix for track angle, 2-17-94 */
  if((z_begin > 0.0) && (z_end < 0.0)) {
    track_angle = 270.0 - track_angle;
  } else if(z_end < 0.0) {
    if (angle2 > angle1 || (angle2 == angle1 && z_middle > 0.0)) {
      track_angle = 270.0 - track_angle;
    } else {
      track_angle = 270.0 + track_angle;
    }
  } else {
    if (angle2 > angle1 || (angle2 == angle1 && z_middle > 0.0)) {
      track_angle = 270.0 + track_angle;
    } else {
      track_angle = 270.0 - track_angle;
    }
  }


  /*********** range_resolution ***********/
  rng_res = 30.0;
  if(request.pixel_spacing == 100.0) rng_res = 150.0;


  /********** slant range of last pixel ***************/
  if(icomplex == 0) {
    /* ground range of 1st pixel */
    sr2gr(&r0_base,&ground_range,
	  &rng_in,&az_in,&nadd,&n_max,&pi,&c,&pxl_unit,
	  &ers,&jers,&rsat_s12,&rsat_s37,&rsat_ws,&rsat_fr,&rsat_eh,&rsat_el,
	  buff_in,array,buff_in_rd,
	  &mode,
	  &raw_mode,&i_dop_mode,&nband,&nlines,&line_offset,
	  &ns,&nr_header,&ns_header,&i_squint,&i_pt_sim,&i_data_type,&nlook,
	  &nfr,&nfx,&icomplex,&i_preproc,&i_iqqi,&nx_offset,&i_dummy,
	  &lambda,&fc,&bw,&dmu,&fs,&plen,&k_chirp,&dr,
	  &prf,&prf_in,&pbw,&az_ang,&az_res,&df,
	  &t_1st_echo,&t_eph,&t_gha,&gha,&xs,&ys,&zs,&rsc,&vxs,&vys,&vzs,&vs,
	  &re_mean,&h,
	  &rs_ref,&r0_base,&fd0_base,&fr0_base,&fd0,&fr0,&fdd,&frd,
	  &fd_const,&r_ref,&r_1st,
	  &pxl_spacing,&nx_spacing,
	  &t_start_frm,&t_end_frm,
	  &lat1,&lon1,&rs_crn1,&rg_crn1,&lookang1,
	  &lat2,&lon2,&rs_crn2,&rg_crn2,&lookang2,
	  &lat3,&lon3,&rs_crn3,&rg_crn3,&lookang3,
	  &lat4,&lon4,&rs_crn4,&rg_crn4,&lookang4,
	  &lat5,&lon5,&rs_crn5,&rg_crn5,&lookang5,
	  raw_file,image_file);
    ground_range += nfr*pxl_spacing;
    gr2sr(&ground_range, &slant_range,
	  &rng_in,&az_in,&nadd,&n_max,&pi,&c,&pxl_unit,
	  &ers,&jers,&rsat_s12,&rsat_s37,&rsat_ws,&rsat_fr,&rsat_eh,&rsat_el,
	  buff_in,array,buff_in_rd,
	  &mode,
	  &raw_mode,&i_dop_mode,&nband,&nlines,&line_offset,
	  &ns,&nr_header,&ns_header,&i_squint,&i_pt_sim,&i_data_type,&nlook,
	  &nfr,&nfx,&icomplex,&i_preproc,&i_iqqi,&nx_offset,&i_dummy,
	  &lambda,&fc,&bw,&dmu,&fs,&plen,&k_chirp,&dr,
	  &prf,&prf_in,&pbw,&az_ang,&az_res,&df,
	  &t_1st_echo,&t_eph,&t_gha,&gha,&xs,&ys,&zs,&rsc,&vxs,&vys,&vzs,&vs,
	  &re_mean,&h,
	  &rs_ref,&r0_base,&fd0_base,&fr0_base,&fd0,&fr0,&fdd,&frd,
	  &fd_const,&r_ref,&r_1st,
	  &pxl_spacing,&nx_spacing,
	  &t_start_frm,&t_end_frm,
	  &lat1,&lon1,&rs_crn1,&rg_crn1,&lookang1,
	  &lat2,&lon2,&rs_crn2,&rg_crn2,&lookang2,
	  &lat3,&lon3,&rs_crn3,&rg_crn3,&lookang3,
	  &lat4,&lon4,&rs_crn4,&rg_crn4,&lookang4,
	  &lat5,&lon5,&rs_crn5,&rg_crn5,&lookang5,
	  raw_file,image_file);
  } else {
    slant_range = r0_base + nfr*dr;
  }
  slant_range_last_pxl = slant_range;



  /* Calculate range spectrum */
  start_sample = (ns-2048)/2;
  start_line = nlines/2;
  decode(&start_sample,&start_line,&raw_file_fd,&raw_data_offset,
	 auxiliary.aux_window_pos_changes, 
	 auxiliary.aux_window_pos_values,
	 auxiliary.aux_agc_changes, auxiliary.aux_agc_values,
	 &rng_in,&az_in,&nadd,&n_max,&pi,&c,&pxl_unit,
	 &ers,&jers,&rsat_s12,&rsat_s37,&rsat_ws,&rsat_fr,&rsat_eh,&rsat_el,
	 buff_in,array,buff_in_rd,
	 &mode,
	 &raw_mode,&i_dop_mode,&nband,&nlines,&line_offset,
	 &ns,&nr_header,&ns_header,&i_squint,&i_pt_sim,&i_data_type,&nlook,
	 &nfr,&nfx,&icomplex,&i_preproc,&i_iqqi,&nx_offset,&i_dummy,
	 &lambda,&fc,&bw,&dmu,&fs,&plen,&k_chirp,&dr,
	 &prf,&prf_in,&pbw,&az_ang,&az_res,&df,
	 &t_1st_echo,&t_eph,&t_gha,&gha,&xs,&ys,&zs,&rsc,&vxs,&vys,&vzs,&vs,
	 &re_mean,&h,
	 &rs_ref,&r0_base,&fd0_base,&fr0_base,&fd0,&fr0,&fdd,&frd,
	 &fd_const,&r_ref,&r_1st,
	 &pxl_spacing,&nx_spacing,
	 &t_start_frm,&t_end_frm,
	 &lat1,&lon1,&rs_crn1,&rg_crn1,&lookang1,
	 &lat2,&lon2,&rs_crn2,&rg_crn2,&lookang2,
	 &lat3,&lon3,&rs_crn3,&rg_crn3,&lookang3,
	 &lat4,&lon4,&rs_crn4,&rg_crn4,&lookang4,
	 &lat5,&lon5,&rs_crn5,&rg_crn5,&lookang5,
	 raw_file,image_file);
  
  tmp_buffer = (double*) malloc(sizeof(double)*rng_in);
  spectrum = (double*) malloc(sizeof(double)*rng_in);
  range_spectrum(buff_in_rd,tmp_buffer,spectrum,&rng_in,&az_in);
  free(tmp_buffer);
  /* map range spectrum from rng_in data points to 256 data points */
  inc = rng_in/256;
  for(i = 0; i < rng_in; i+=inc) {
    average = 0.0;
    for(j = 0; j < inc; j++) {
      average += spectrum[i+j];
    }
    average /= inc;
    spectrum[i/inc] = average;
  }

  /****** we have all the information.  we write it out ********/

  fptr = fopen(file_name, "w");
  if(fptr == NULL) {
    info_handler(ierr_2,file_name, "Failed to create file %s for writing",
		 file_name);
  }

  fprintf(fptr, "SPS_PP_INFO\n");
  fprintf(fptr, "[IMAGE]\n");
  fprintf(fptr, "%7d    RECORD_LENGTH  \n",record_length);
  fprintf(fptr, "%7d    NUMBER_OF_RECORDS  \n",number_of_records);
  fprintf(fptr, "[DATA_SET_SUMMARY]\n");
  fprintf(fptr, "%s    INP_SCTIM  \n",gmt_digit_only);
  fprintf(fptr, "%16.7lf    PRO_LAT  \n",lat5);
  fprintf(fptr, "%16.7lf    PRO_LONG  \n",lon5);
  fprintf(fptr, "%16.7lf    PRO_HEAD  \n",platform_heading);
  fprintf(fptr, "%16.7lf    TERRAIN_H  \n",h);
  fprintf(fptr, "%16.7lf    SCENE_LEN  \n",pxl_spacing*nfx/1000);
  fprintf(fptr, "%16.7lf    SCENE_WID  \n",pxl_spacing*nfr/1000);
  fprintf(fptr, "%8.3f    PLAT_LAT  \n",lat5);
  fprintf(fptr, "%8.3f    PLAT_LONG  \n",lon5);
  fprintf(fptr, "%8.3f    PLAT_HEAD_SCENE  \n",platform_heading);
  fprintf(fptr, "%8.3f    INCIDENT_ANG  \n",incident_angle);
  fprintf(fptr, "%16.7lf    WAVE_LENGTH  \n",lambda);
  fprintf(fptr, "%16.7e    AMPL_COEF0  \n",zero);
  fprintf(fptr, "%16.7e    AMPL_COEF1  \n",zero);
  fprintf(fptr, "%16.7e    AMPL_COEF2  \n",zero);
  fprintf(fptr, "%16.7e    AMPL_COEF3  \n",zero);
  fprintf(fptr, "%16.7e    AMPL_COEF4  \n",zero);
  fprintf(fptr, "%16.7e    PHAS_COEF0  \n",zero);
  fprintf(fptr, "%16.7e    PHAS_COEF1  \n",k_chirp);
  fprintf(fptr, "%16.7e    PHAS_COEF2  \n",zero);
  fprintf(fptr, "%16.7e    PHAS_COEF3  \n",zero);
  fprintf(fptr, "%16.7e    PHAS_COEF4  \n",zero);
  fprintf(fptr, "%8d    CHIRP_EXT_IND  \n",(int)(fs*plen));
  fprintf(fptr, "%16.7lf    RNG_SAMP_RATE  \n",fs/1000000.0);
  fprintf(fptr, "%16.7lf    RNG_GATE  \n",auxiliary.aux_window_st_time);
  fprintf(fptr, "%16.7e     RNG_LENGTH  \n",plen*1000000.0);
  fprintf(fptr, "%16.7lf    ELE_SIGHT  \n",look_angle_local);
  fprintf(fptr, "%16.7lf    PRF  \n",prf);
  fprintf(fptr, "%16.7lf    AZI_BEAM  \n",az_ang);
  if(i_squint == 0) {
    fprintf(fptr, "RANGE DOPPLER    ALGOR_ID  \n");
  } else if(i_squint == 1) {
    fprintf(fptr, "CHIRP SCALING    ALGOR_ID  \n");
  }
  fprintf(fptr, "%16.7lf    N_AZILOK  \n",(double)nlook);
  fprintf(fptr, "%16.7lf    N_RNGLOK  \n",one);
  fprintf(fptr, "%16.7lf    BND_AZILOK  \n",pbw);
  fprintf(fptr, "%16.7lf    BND_RNGLOK  \n",bw);
  fprintf(fptr, "%16.7lf    BND_AZI  \n",pbw);
  fprintf(fptr, "%16.7lf    BND_RNG  \n",bw);
  fprintf(fptr, "%16.7lf    RNG_RES  \n",rng_res);
  fprintf(fptr, "%16.7lf    AZI_RES  \n",az_res);
  fprintf(fptr, "%16.7e    ALT_DOPCEN0  \n",fd0_base);
  fprintf(fptr, "%16.7e    ALT_DOPCEN1  \n",zero);
  fprintf(fptr, "%16.7e    ALT_DOPCEN2  \n",zero);
  fprintf(fptr, "%16.7e    CRT_DOPCEN0  \n",fd0_base);
  fprintf(fptr, "%16.7e    CRT_DOPCEN1  \n",fdd*dr);
  fprintf(fptr, "%16.7e    CRT_DOPCEN2  \n",zero);
  fprintf(fptr, "%16.7e    ALT_RATE0  \n",fr0_base);
  fprintf(fptr, "%16.7e    ALT_RATE1  \n",zero);
  fprintf(fptr, "%16.7e    ALT_RATE2  \n",zero);
  fprintf(fptr, "%16.7e    CRT_RATE0  \n",fr0_base);
  fprintf(fptr, "%16.7e    CRT_RATE1  \n",frd*dr);
  fprintf(fptr, "%16.7e    CRT_RATE2  \n",zero);
  if(clutter_flag)  
    fprintf(fptr, " YES    CLUTTERLOCK_FLG  \n");
  else
    fprintf(fptr, "  NO    CLUTTERLOCK_FLG  \n");
  if(auto_focus_flag)
    fprintf(fptr, " YES    AUTO_FOCUS  \n");
  else
    fprintf(fptr, "  NO    AUTO_FOCUS  \n");
  if(icomplex == 0) {
    fprintf(fptr, "%16.7lf    LINE_SPACING  \n",request.pixel_spacing);
    fprintf(fptr, "%16.7lf    PIXEL_SPACING  \n",request.pixel_spacing);
  }
  if(icomplex == 1) {
    fprintf(fptr, "%16.7lf    LINE_SPACING  \n",nx_spacing);
    fprintf(fptr, "%16.7lf    PIXEL_SPACING  \n",dr);
  }
  if(icomplex == 2) {
    fprintf(fptr, "%16.7lf    LINE_SPACING  \n",request.pixel_spacing);
    fprintf(fptr, "%16.7lf    PIXEL_SPACING \n",dr);
  }
  fprintf(fptr, "%8.3lf    PRF1 \n",prf);
  fprintf(fptr, "%8.3lf    RNG_GATE1 \n",auxiliary.aux_window_st_time);
  fprintf(fptr, "[FACILITY_RELATED_RECORD]\n");
  fprintf(fptr, "%s    CENTER_GMT \n",gmt);
  fprintf(fptr, "%16.7lf    CENTER_LAT \n",lat5);
  fprintf(fptr, "%16.7lf    CENTER_LON \n",lon5);
  fprintf(fptr, "%16.7lf    NEAR_START_LAT \n",lat1);
  fprintf(fptr, "%16.7lf    NEAR_START_LON \n",lon1);
  fprintf(fptr, "%16.7lf    NEAR_END_LAT \n",lat3);
  fprintf(fptr, "%16.7lf    NEAR_END_LON \n",lon3);
  fprintf(fptr, "%16.7lf    FAR_START_LAT \n",lat2);
  fprintf(fptr, "%16.7lf    FAR_START_LON \n",lon2);
  fprintf(fptr, "%16.7lf    FAR_END_LAT \n",lat4);
  fprintf(fptr, "%16.7lf    FAR_END_LON \n",lon4);
  fprintf(fptr, "%16.7lf    NADIR_RADIUS \n",nadir_mean_earth_radius/1000.0);
  fprintf(fptr, "%16.7lf    IMAGE_RADIUS \n",image_mean_earth_radius/1000.0);
  fprintf(fptr, "%16.7lf    S_C_ALTITUDE \n",(rsc-nadir_mean_earth_radius)/1000.0);
  fprintf(fptr, "%16.7lf    SL_RNG_1ST_PIX \n",r0_base/1000.0);
  fprintf(fptr, "%16.7lf    SL_RNG_MID_PIX \n",slant_range_mid_pxl/1000.0);
  fprintf(fptr, "%16.7lf    SL_RNG_LAST_PIX \n",slant_range_last_pxl/1000.0);
  
  fprintf(fptr, "%16.7lf    TRACK_ANGLE \n",track_angle);

  fprintf(fptr, "%16.7lf    DOP_FRQ_CONST \n",fd0_base);
  if(icomplex == 1) {
    fprintf(fptr, "%16.7lf    DOP_FRQ_SLOPE \n",fdd*nx_spacing);
  } else {
    fprintf(fptr, "%16.7lf    DOP_FRQ_SLOPE \n",fdd*request.pixel_spacing);
  }
  fprintf(fptr, "%16.7lf    DOP_FRQ_QUAD \n",zero);

  fprintf(fptr, "%16.7lf    SQUINT_ANGLE \n",yaw);
  fprintf(fptr, "%16.7lf    SWATH_VELOCITY \n",vsg);
  fprintf(fptr, "%s    START_TIME \n",start_gmt);
  fprintf(fptr, "%s    END_TIME \n",end_gmt);

  fprintf(fptr, "[STATE_VECTOR_AT_FIRST_ECHO]\n");
  fprintf(fptr, "%22.15lf    X_POSITION (kilometers) \n",xs/1000.);
  fprintf(fptr, "%22.15lf    Y_POSITION (kilometers) \n",ys/1000.);
  fprintf(fptr, "%22.15lf    Z_POSITION (kilometers) \n",zs/1000.);
  fprintf(fptr, "%22.15lf    X_VELOCITY (meters/second) \n",vxs);
  fprintf(fptr, "%22.15lf    Y_VELOCITY (meters/second) \n",vys);
  fprintf(fptr, "%22.15lf    Z_VELOCITY (meters/second) \n",vzs);

  fprintf(fptr, "[STATE_VECTOR_AT_BEGINNING_OF_FRAME]\n");
  fprintf(fptr, "%22.15lf    X_POSITION (kilometers) \n",x_begin/1000.);
  fprintf(fptr, "%22.15lf    Y_POSITION (kilometers) \n",y_begin/1000.);
  fprintf(fptr, "%22.15lf    Z_POSITION (kilometers) \n",z_begin/1000.);
  fprintf(fptr, "%22.15lf    X_VELOCITY (meters/second) \n",vx_begin);
  fprintf(fptr, "%22.15lf    Y_VELOCITY (meters/second) \n",vy_begin);
  fprintf(fptr, "%22.15lf    Z_VELOCITY (meters/second) \n",vz_begin);

  fprintf(fptr, "[STATE_VECTOR_AT_MIDDLE_OF_FRAME]\n");
  fprintf(fptr, "%22.15lf    X_POSITION (kilometers) \n",x_middle/1000.);
  fprintf(fptr, "%22.15lf    Y_POSITION (kilometers) \n",y_middle/1000.);
  fprintf(fptr, "%22.15lf    Z_POSITION (kilometers) \n",z_middle/1000.);
  fprintf(fptr, "%22.15lf    X_VELOCITY (meters/second) \n",vx_middle);
  fprintf(fptr, "%22.15lf    Y_VELOCITY (meters/second) \n",vy_middle);
  fprintf(fptr, "%22.15lf    Z_VELOCITY (meters/second) \n",vz_middle);

  fprintf(fptr, "[STATE_VECTOR_AT_END_OF_FRAME]\n");
  fprintf(fptr, "%22.15lf    X_POSITION (kilometers) \n",x_end/1000.);
  fprintf(fptr, "%22.15lf    Y_POSITION (kilometers) \n",y_end/1000.);
  fprintf(fptr, "%22.15lf    Z_POSITION (kilometers) \n",z_end/1000.);
  fprintf(fptr, "%22.15lf    X_VELOCITY (meters/second) \n",vx_end);
  fprintf(fptr, "%22.15lf    Y_VELOCITY (meters/second) \n",vy_end);
  fprintf(fptr, "%22.15lf    Z_VELOCITY (meters/second) \n",vz_end);

  /* qdn 2/19/98 the interval between state vectors */
  delta_t_middle = (t_end_frm  - t_start_frm)/2;
  fprintf(fptr, "%16.7lf    DATA_INT \n",delta_t_middle);

  fprintf(fptr, "[RANGE_SPECTRA]\n");
  for(i = 0; i < 256; i+=4) {
    fprintf(fptr, "%16.7lf %16.7lf %16.7lf %16.7lf\n",spectrum[i],spectrum[i+1],spectrum[i+2],spectrum[i+3]);
  }

  fclose(fptr);
}




