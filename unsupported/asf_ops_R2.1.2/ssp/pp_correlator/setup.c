/* SccsId[]= @(#)setup.c	2.41 3/24/98 */
static char sccsid_setup[]= "@(#)PPsetup.c:2.41";

#include "pp.h"

void setup(char * frame_request_file)
{
  /******* local variables *******/
  double xt,yt,zt,px,py,pz,t_spacing,delta_t;
  int i, found, ramp_product_flag, error_code, n_pulse_in_air, rst;
  char *tmp_buffer, error_msg[MAX_LENGTH], filename[MAX_LENGTH];
  double cx, cy, cz,theta, phi;

  /************** some constants *******************/
  pi = 3.1415926535897938238;
  c = 299792458.0;
  ers = 1;
  jers = 2;
  rsat_s12 = 3;
  rsat_s37 = 4;
  rsat_ws = 5;
  rsat_fr = 6;
  rsat_eh = 7;
  rsat_el = 8;
  
  /**************** some hard coded variables ******************/
  mode = 2;      /* 1 for air sar, 2 for space sar */
  i_pt_sim = 0;
  i_preproc = 1;  /* 1 for preprocessing, 0 for no preprocessing */
  line_offset = 0;
  look_ang_cnt = 29.8; 
  ele_ang_ref = -20.08;  /*Elevation angle reference in ant files*/
  new_elev_incr = .0575; /*Range angle spacing in antenna pattern*/

  /***************** read frame request file ********************/
  if((error_code = get_frame_rqst(frame_request_file, 
				 &request,
				 error_msg)) != 0) {
    info_handler(error_code, NULL, "%s", error_msg);
  }


  /***************** read auxiliary sps file ********************/
  /* build auxiliary file name from job id */
  sprintf(filename, "/home/tmpdisk/aux.dat.%d", request.job_id);
  if((error_code = get_auxiliary(filename,
				&auxiliary,
				error_msg)) != 0) {
    info_handler(error_code, NULL, "%s", error_msg);
  }


  /***************** read ephemeris file ********************/
  /* build ephemeris file name from job id */
  /*
  sprintf(filename, "/home/tmpdisk/eph.dat.%d", request.job_id);
  if((error_code = get_ephemeris(filename,
				&ephemeris,
				error_msg)) != 0) {
    info_handler(error_code, NULL, "%s", error_msg);
  }
  */

  /****** read raw data file and search for the beginning of data *****/
  /* build raw data file name from job id. */
  sprintf(raw_file, "/home/tmpdisk/echo.dat.%d", request.job_id);
  if ((raw_file_fd = open(raw_file, O_RDONLY)) == -1) {
    info_handler(ierr_2, raw_file, "setup: open file %s failed", raw_file);
  }

  if((tmp_buffer = (char*) malloc(sizeof(char)*1024)) == NULL) {
    info_handler(ierr_2, NULL, "setup: allocate memory failed");
  }

  if(read(raw_file_fd, tmp_buffer, sizeof(char)*1024) != sizeof(char)*1024) {
    info_handler(ierr_2, NULL, "setup: read file %s failed", raw_file);
  }

  found = 0;
  for(i = 0; i < 1024-7; i++) {
    if(tmp_buffer[i] == 'E' && tmp_buffer[i+1] == 'C' && 
       tmp_buffer[i+2] == 'H' && tmp_buffer[i+3] == 'O' &&
       tmp_buffer[i+4] == 'R' && tmp_buffer[i+5] == 'A' &&
       tmp_buffer[i+6] == 'W') {
      raw_data_offset = i + 32;
      found = 1;
      break;
    }
  }

  printf("raw_data_offset %d\n",raw_data_offset);

  free(tmp_buffer);
  close(raw_file_fd);
  if(found != 1) {
    info_handler(ierr_4, NULL, "Cannot find string ECHORAW in file %s",
		 raw_file);
  }


  /**************** determine the mode ******************/
  switch(request.platform[0]) {
  case 'E':
    /* ERS */
    i_data_type = ers;
    break;
  case 'J':
    /* JERS */
    i_data_type = jers;
    break;
  case 'R':
    /* RADARSAT */
    if(request.instr_mode[0] == 'S') {
      /* STANDARD */
      if(request.instr_mode[2] >= '1' && request.instr_mode[2] <= '2') {
	i_data_type = rsat_s12;
      } else {
	if(request.instr_mode[2] >= '3' && request.instr_mode[2] <= '7') {
	  i_data_type = rsat_s37;
	} else {
	  info_handler(ierr_4,NULL,"in %s, unknown instrument mode %s",
		       frame_request_file, request.instr_mode);
	}
      }
    } else {
      if(request.instr_mode[0] == 'W') {
	/* WS */
	i_data_type = rsat_ws;
      } else {
	if(request.instr_mode[0] == 'F') {
	  /* FR */
	  i_data_type = rsat_fr;
	} else {
	  if(request.instr_mode[0] == 'E' && request.instr_mode[1] == 'H') {
	    i_data_type = rsat_eh;
	  } else {
	    if(request.instr_mode[0] == 'E' && request.instr_mode[1] == 'L') {
	      i_data_type = rsat_el;
	    } else {
	      info_handler(ierr_4,NULL,"in %s, unknown instrument mode %s",
			   frame_request_file, request.instr_mode);
	    }
	  }
	}
      }
    }
    break;
  default:
      info_handler(ierr_4,NULL,"in %s, unkown platform %s", 
		   frame_request_file, request.platform);
  }

  /**************** determine product type ******************/
  ramp_product_flag = 0;
  if(strncmp(request.prod_type,"STANDARD",8) == 0) {
    icomplex = 0;
    i_dop_mode = 1;
  } else if(strncmp(request.prod_type,"COMPLEX",7) == 0) {
    icomplex = 1;
    i_dop_mode = 1;
  } else if(strncmp(request.prod_type,"RAMP",4) == 0) {
    icomplex = 2;
    i_dop_mode = 3;
    ramp_product_flag = 1;
  } else {
    info_handler(ierr_4,NULL,"in frame request file, unknown product type %s",
		 request.prod_type);
  }

  /********** convert time from strings to julian seconds *********/ 
  gmt2sec(auxiliary.aux_sc_time, &t_1st_echo);
  gmt2sec(request.sv_time, &t_eph);
  gmt2sec(request.gha_corr_time, &t_gha);

  /* adjustment for radarsat to compensate geo-location errors */
  if(request.platform[0] == 'R') {
    t_1st_echo -= 0.0135;
    auxiliary.aux_window_st_time -= 3.9e-6; 
  }

  /******* propagate the state vector from t_eph to t_1st_echo *******/
  t_spacing = 2.e-4;
  delta_t = t_1st_echo - t_eph;
  /* first convert the numbers from kms to meters */
  request.sv_x_pos *= 1000.0;
  request.sv_y_pos *= 1000.0;
  request.sv_z_pos *= 1000.0;

  /*
  propa_simp(&request.sv_x_pos,&request.sv_y_pos,&request.sv_z_pos,
	     &request.sv_x_vel,&request.sv_y_vel,&request.sv_z_vel,
	     &delta_t,&t_spacing,&xs,&ys,&zs,&vxs,&vys,&vzs);
	     */

  propagation(&request.sv_x_pos,&request.sv_y_pos,&request.sv_z_pos,
	      &request.sv_x_vel,&request.sv_y_vel,&request.sv_z_vel,
	      &t_eph,&delta_t,&xs,&ys,&zs,&vxs,&vys,&vzs);


  /************* set other parameters based on the data type *****/
  if(i_data_type == ers) {
    i_iqqi = 0;
    nlook = 4;
    pxl_spacing = 12.5;
    az_res = 30.;
    nfr = 7600;
    nfx = 8192;
    bw = 15550000.0;
    lambda = .05656;
    plen=.0000371;
    k_chirp=bw/plen;
    az_ang=0.20;
    rng_in = 2048;
    az_in = 4096;
    nadd = az_in/4;
    n_max = (rng_in > az_in) ? rng_in : az_in;
    pxl_unit = 6.25;
    nr_header = 0;
    raw_mode = 2;
    look_angle = (20.3/180.0)*pi;
  }

  if(i_data_type == jers) {
    i_iqqi = 0;
    nlook = 4;
    pxl_spacing = 12.5;
    az_res = 30.;
    nfr = 7600;
    nfx = 8192;
    bw = 15000000.0; 
    lambda = .23513;
    plen=.000035;
    k_chirp=-bw/plen; 
    az_ang=1.0;
    rng_in = 2048;
    az_in = 4096;
    nadd = az_in/4;
    n_max = (rng_in > az_in) ? rng_in : az_in;
    pxl_unit = 6.25;
    nr_header = 0;
    raw_mode = 2;
    look_angle = (35.0/180.0)*pi;
  }

  if(i_data_type == rsat_s12 || i_data_type == rsat_s37) {
    i_iqqi = 0;
    nlook = 4;
    pxl_spacing = 12.5;
    az_res = 25.;
    nfr = 8192;
    nfx = 8192;	
    if(i_data_type == rsat_s12) {
      bw = 17480000.0;
      lambda = .0565646;
      plen=.000042;
      k_chirp=-bw/plen; 
      az_ang=0.20;
    }
    if(i_data_type == rsat_s37) {
      /*bw = 11580000.0; */
      bw = 11780000.0; 
      lambda = .0565646;
      plen=.000042; 
      k_chirp=-bw/plen; 
      az_ang=0.20;
    }
    rng_in = 2048;
    az_in = 4096;
    nadd = az_in/4;
    n_max = (rng_in > az_in) ? rng_in : az_in;
    pxl_unit = 6.25;
    nr_header = 0;
    raw_mode = 2;
    if(request.instr_mode[2] == '1') look_angle = (17.30)/(180.0)*pi;
    if(request.instr_mode[2] == '2') look_angle = (21.34)/(180.0)*pi;
    if(request.instr_mode[2] == '3') look_angle = (26.72)/(180.0)*pi;
    if(request.instr_mode[2] == '4') look_angle = (29.42)/(180.0)*pi;
    if(request.instr_mode[2] == '5') look_angle = (31.86)/(180.0)*pi;
    if(request.instr_mode[2] == '6') look_angle = (35.95)/(180.0)*pi;
    if(request.instr_mode[2] == '7') look_angle = (38.72)/(180.0)*pi;
  }

  if(i_data_type == rsat_ws) {
    i_iqqi = 0;
    nlook = 4;
    pxl_spacing = 25.;
    az_res = 35.;
    nfr = 6000;
    nfx = 6000;
    bw = 11780000.0;
    lambda = .0565646; 
    plen=.000042;
    k_chirp=-bw/plen; 
    az_ang=0.20;
    rng_in = 2048;
    az_in = 4096;
    nadd = az_in/4;
    n_max = (rng_in > az_in) ? rng_in : az_in;
    pxl_unit = 6.25;
    nr_header = 0;
    raw_mode = 2;
    if(request.instr_mode[2] == '1') look_angle = (17.65)/(180.0)*pi;
    if(request.instr_mode[2] == '2') look_angle = (27.01)/(180.0)*pi;
    if(request.instr_mode[2] == '3') look_angle = (34.12)/(180.0)*pi;
  }

  if(i_data_type == rsat_fr) {
    i_iqqi = 0;
    nlook = 1;
    pxl_spacing = 6.25;
    az_res = 10.;
    nfr = 8192;
    nfx = 8192;
    bw = 30002000.0; 
    lambda = .0565646; 
    plen=.000042; 
    k_chirp=-bw/plen;
    az_ang=0.20; 
    rng_in = 2048;
    az_in = 4096;
    nadd = az_in/4;
    n_max = (rng_in > az_in) ? rng_in : az_in;
    pxl_unit = 6.25;
    nr_header = 0;
    raw_mode = 2;
    if(request.instr_mode[2] == '1') look_angle = (32.19)/(180.0)*pi;
    if(request.instr_mode[2] == '2') look_angle = (34.23)/(180.0)*pi;
    if(request.instr_mode[2] == '3') look_angle = (36.06)/(180.0)*pi;
    if(request.instr_mode[2] == '4') look_angle = (37.66)/(180.0)*pi;
    if(request.instr_mode[2] == '5') look_angle = (39.14)/(180.0)*pi;
  }

  if(i_data_type == rsat_eh) {
    i_iqqi = 0;
    nlook = 4;
    pxl_spacing = 12.5;
    az_res = 25.;
    nfr = 8192;
    nfx = 8192;
    bw = 11780000.0; 
    lambda = .0565646; 
    plen=.000042; 
    k_chirp=-bw/plen; 
    az_ang=0.20;
    rng_in = 2048;
    az_in = 4096;
    nadd = az_in/4;
    n_max = (rng_in > az_in) ? rng_in : az_in;
    pxl_unit = 6.25;
    nr_header = 0;
    raw_mode = 2;
    if(request.instr_mode[2] == '1') look_angle = (41.93)/(180.0)*pi;
    if(request.instr_mode[2] == '2') look_angle = (43.08)/(180.0)*pi;
    if(request.instr_mode[2] == '3') look_angle = (44.16)/(180.0)*pi;
    if(request.instr_mode[2] == '4') look_angle = (46.20)/(180.0)*pi;
    /*if(request.instr_mode[2] == '4') look_angle = (41.93)/(180.0)*pi;*/
    if(request.instr_mode[2] == '5') look_angle = (47.23)/(180.0)*pi;
    if(request.instr_mode[2] == '6') look_angle = (48.28)/(180.0)*pi;
  }
	
  if(i_data_type == rsat_el) {
    i_iqqi = 0;
    nlook = 4;
    pxl_spacing = 25.;
    az_res = 50.;
    nfr = 6800;
    nfx = 6800;
    bw = 11780000.0; 
    lambda = .0565646; 
    plen=.000042; 
    k_chirp=-bw/plen; 
    az_ang=0.20; 
    rng_in = 2048;
    az_in = 4096;
    nadd = az_in/4;
    n_max = (rng_in > az_in) ? rng_in : az_in;
    pxl_unit = 6.25;
    nr_header = 0;
    raw_mode = 2;
    look_angle = (8.8)/(180.0)*pi;
  }


  /*********** set variables ***********/
  sprintf(image_file, "/home/tmpdisk/image_tmp.dat.%d", request.job_id);
  sprintf(info_file, "/home/tmpdisk/sps_pp_info.%d", request.job_id);
  h = request.avg_terrain;
  gha = request.gha_corr_angle;
  ns = auxiliary.aux_echo_samples;
  nlines = auxiliary.aux_echo_pulses;
  fs = auxiliary.aux_sampling_rate;
  prf = 1.0/auxiliary.aux_pulse_period;
  if(strncmp(request.frame_mode, "ARCTIC", 6) == 0)
    frame_mode = 0;
  else if(strncmp(request.frame_mode, "ANTARCTIC", 9) == 0)
    frame_mode = 1;
  else
    info_handler(ierr_22,NULL,"undefined frame_mode"); 

  /* initialize look angles at four corners and the image center */
  lookang1 = lookang2 = lookang3 = lookang4 = lookang5 = look_angle*180.0/pi;


  if(ramp_product_flag == 1) {
    pxl_spacing = 25.0;
    nfx = 4096;
    nfr = 2*((ns - (int)(plen*fs))/2);
  }

  /************ find the unit vector pointing to the target **************/
  /* unit vector perpendicular to the vehicle position-velocity plane */
  cross(&vxs,&vys,&vzs,&xs,&ys,&zs,&cx,&cy,&cz);
  /* unit vector pointing to nadir */
  rsc = sqrt(xs*xs+ys*ys+zs*zs);
  vs = sqrt(vxs*vxs+vys*vys+vzs*vzs);
  px = -xs/rsc;
  py = -ys/rsc;
  pz = -zs/rsc;
  /* unit vector pointing to the target */
  px = px*cos(look_angle) + cx*sin(look_angle);
  py = py*cos(look_angle) + cy*sin(look_angle);
  pz = pz*cos(look_angle) + cz*sin(look_angle);


  /******** other parameters **********/
  ns_header = ns+nr_header;
  pbw = prf*0.8;
  fc=c/lambda; 
  dr = c/(2*fs); 
  df = prf/az_in;
  dmu = fs/rng_in; 

  seetarg(&h,&xs,&ys,&zs,&px,&py,&pz,&xt,&yt,&zt,&re_mean);

  /********* calculate r0_base **************/
  /*
  n_pulse_in_air = 0.0;
  if(instr_mode[0] == 'S' && instr_mode[2] == '1') n_pulse_in_air = 7.0;
  if(instr_mode[0] == 'S' && instr_mode[2] == '2') n_pulse_in_air = 7.0;
  if(instr_mode[0] == 'S' && instr_mode[2] == '3') n_pulse_in_air = 8.0;
  if(instr_mode[0] == 'S' && instr_mode[2] == '4') n_pulse_in_air = 8.0;
  if(instr_mode[0] == 'S' && instr_mode[2] == '5') n_pulse_in_air = 8.0;
  if(instr_mode[0] == 'S' && instr_mode[2] == '6') n_pulse_in_air = 9.0;
  if(instr_mode[0] == 'S' && instr_mode[2] == '7') n_pulse_in_air = 9.0;
  */

  /*
  rsc = sqrt(xs*xs+ys*ys+zs*zs);
  theta = asin(sin(look_angle)*rsc/re_mean);
  phi = theta - look_angle;
  rst = sin(phi)*rsc/sin(theta);
  n_pulse_in_air = rst*prf*2/c;
  */
  rsc = sqrt(xs*xs+ys*ys+zs*zs);
  theta = 2*rsc*cos(look_angle);
  rst = (theta - sqrt(theta*theta-4*(rsc*rsc-re_mean*re_mean)))*.5;
  n_pulse_in_air = rst*prf*2.0/c;

  info_handler(0, NULL, "n_pulse_in_air %d, look angle %f", 
	       n_pulse_in_air, look_angle*180/pi); 

  r0_base = c*(auxiliary.aux_window_st_time + n_pulse_in_air/prf)/2.0;

  /* take into account of window position change */
  if(auxiliary.aux_window_pos_changes[1] > 0) {
    info_handler(0,NULL,"r0_base changed to %f due to window position change",
		 r0_base);
    r0_base = r0_base + abs(auxiliary.aux_window_pos_values[1]-
			    auxiliary.aux_window_pos_values[0])*c/2.0;
  }

  r_fd_fr_init(&rng_in,&az_in,&nadd,&n_max,&pi,&c,&pxl_unit,
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
	    raw_file,image_file,&frame_mode);


  i_squint = 0;

  if((lambda*fd0_base*bw)*(lambda*fd0_base*bw)/(4.0*fr0_base*c*c) >= 1) {
    i_squint = 1;
  }
  
  if(i_squint == 1) {
    info_handler(0,NULL,"Use Chirp Scaling Algorithm for Image Processing");
    i_dop_mode = 2;
  } else {
    info_handler(0,NULL,"Use Range Doppler Algorithm for Image Processing");
  }

  fd_const=(int)((fd0_base+(ns-plen*fs)*dr*fdd/2)/500.+0.5)*500.0;

  if(icomplex == 1) {
    nfr = (int)(((ns - plen*fs)/100)+1)*100;
    nfx = nlines - n_az_ref - 128;
  }

  /* reinitialize look angles at four corners and the image center */
  lookang1 = lookang2 = lookang3 = lookang4 = lookang5 = look_angle*180.0/pi;

  info_handler(0,NULL,"fd0_base %f, fdd %f, fr0_base %f, frd %f, r0_base %f",
	fd0_base,fdd,fr0_base,frd,r0_base);
}
