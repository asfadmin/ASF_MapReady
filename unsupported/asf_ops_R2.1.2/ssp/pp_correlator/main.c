/* SccsId[]= @(#)main.c	2.41 3/24/98 */
static char sccsid_main[]= "@(#)PPmain.c:2.41";

#undef MERGE_8_BIT
#undef MERGE_16_BIT
/*#define MERGE_16_BIT*/

/* fool the compiler, so that there is no need to link with libc_r
   called by libsps.a
*/
void localtime_r() {}

/*float dtime(float *p) { return 0.0;}*/

/*
  parallel precision processor based on m. jin's fortran program.  original 
  fortran program was written by m. jin at jpl on 10/20/96.  the parallel  
  version was written by shelby yang at jpl on 1/30/97
*/

#define _PP_MAIN_
#include "pp.h"

int main(int argc, char **argv)
{
  int my_processor_number, number_of_processors;
  int compu_proc_number, number_of_compu_procs;

  float w[64*128];          /*buffer for 4pt interp weights*/

  /*************other local variables**********************************/
  int i,j,ijk,nrs;
  int loopr,loopx,ir_st,ix_st,nr,nx,ipass;
  int nrw,nr_skip,nx_skip;
  int ir_loc,ia_loc_d2,na_smp,ireset,start_sample,start_line;

  int number_of_tiles, block_size, remainder, assigned_processor, 
    image_buffer_size, tag, record_length, number_of_records,
    clutter_flag,auto_focus_flag, ant_fd;
  double tt1, tt2, ratio, delta_t1, delta_t2, *scale; 
  float *antenna_pattern_gain;
  double *ant_gain_vect, *look_ang_vect;
  float time[2], time1[2];
  int header_offset, finished_blocks;
  int info[2], error_code, counter;
  int inner, outer, loop_inner, loop_outer;
  char dummy[MAX_LENGTH], error_msg[MAX_LENGTH];
  char Dop_log_file[256];

  char *tmp_buffer, *error_ptr;
  int ii, jj, new_nfr, new_nfx, nadd_error, i_dop_mode_old;
  double fd0_base_old, fdd_old, fr0_base_old, frd_old,
    image_mean_earth_radius, nadir_mean_earth_radius;

  /* qdn 1/12/98 for preprocessor modification */
  int fac_reduce;
  double dr_preproc,fs_preproc;

  /* qdn 2/9/98 for preprocessor using prf amb. algorithm */
  int    new_preproc,ir_st_amb,ix_st_amb,start_sample_amb,start_line_amb,n_prf;
  double fd0_amb,fdd_amb,fd_pred;

  FILE *fptr;

  /**** initialize mpi. should be the first line of executable part ********/
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &number_of_processors);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_processor_number);
  MPI_Barrier(MPI_COMM_WORLD);

  if(argc < 2) 
    info_handler(ierr_1, NULL, "Usage: %s eng_config_file", argv[0]);

  info_handler(0,NULL,"number_of_tasks %d", number_of_processors);
  info_handler(0,NULL,"my_task_number %d", my_processor_number);

  dtime(time);
  tt1 = rtc();

  /************** get processing inputs ************************************/

  /* the first argument is the engineering configuration file name */
  if((fptr = fopen(argv[1],"r")) == NULL) {
    info_handler(ierr_2,argv[1],"open %s failed", argv[1]);
  }
  for(i = 0; i < 6; i++) fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", frame_request_file_name);
  fscanf(fptr, "%s", status_file);
  for(i = 0; i < 12; i++) fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%d", &antenna_flag);
  fscanf(fptr, "%s", error_file);
  fclose(fptr);


  /******** execution status **********/
  if(my_processor_number == 0) {
    if((fptr = fopen(status_file, "a")) == NULL) {
      info_handler(ierr_2, status_file, "open %s failed", status_file);
    }
    fprintf(fptr,"3 -0.01\n");
    fflush(fptr);
    finished_blocks = 0;
  }


  /******** setup all the variables **********/   
  setup(frame_request_file_name);

  /* used when not calling setup 
  nfx = 4096;
  nfr = 6520;
  request.job_id = 32602;
  rng_in = 2048;
  az_in = 4096;
  nadd = az_in/4;
  request.comp_flag = malloc(256);
  strcpy(request.comp_flag, "YES");
  */

  /********** open files for input and output ************/
  /*somehow the file has to be opened instead of created for mmap to work */

  image_file_fd = open(image_file, O_RDWR|O_CREAT|O_TRUNC, 0664);
  if(image_file_fd < 0) {
    info_handler(ierr_2, image_file, "main: open image file %s failed.",
		 image_file);
  }

  raw_file_fd = open(raw_file,O_RDONLY,0);
  if(raw_file_fd < 0) {
    info_handler(ierr_2, raw_file, "main: open raw data file %s failed",
		 raw_file);
  }
  
  /******** allocate and initialize buffer for 8 bit image ********/
  if(icomplex == 0) {
    record_length = nfr;
    number_of_records = nfx;
    image_buffer_size = nfx*nfr;
    image_buffer = (char*) malloc(image_buffer_size);
    if(image_buffer == NULL) {
      info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.", 
		   image_buffer_size);
    } 
    for(i = 0; i < image_buffer_size; i++) image_buffer[i] = 0;
  }


  /********* allocate and initialize buffer for 16 bit image *********/
  if(icomplex == 2) {
    record_length = nfr*sizeof(short);
    number_of_records = nfx;
    image_buffer_size = nfx*nfr*sizeof(short);
    image_buffer = (char*) malloc(image_buffer_size);
    if(image_buffer == NULL) {
      info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.", 
		   image_buffer_size);
    } 
    for(i = 0; i < image_buffer_size; i++) image_buffer[i] = 0;
  }


  /********* allocate and initialize 32 bit file space *********/
  if(icomplex == 1) {
    /* each pixel is 4 bytes.  each row has extra 192 bytes for the header */
    /* there are nfx+1 rows */
    header_offset = 192/(2*sizeof(short));
    record_length = (header_offset+nfr)*2*sizeof(short);
    number_of_records = nfx+1;
    image_buffer_size = (header_offset+nfr)*(nfx+1)*2*sizeof(short);
    /* temp buffer for initializing disk space */
    if((image_buffer = (char*) malloc(record_length)) == NULL) {
      info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.", 
		   record_length);
    }
    for(i = 0; i < record_length; i++)
      image_buffer[i] = 0;
    for(i = 0; i < number_of_records; i++) {
      if(write(image_file_fd, image_buffer,record_length) != record_length) {
	info_handler(ierr_2,image_file,
		     "task %d cannot initialize disk space for image file %s",
		     my_processor_number, image_file);
      }
    }
    free(image_buffer);
    image_buffer = mmap(0, image_buffer_size, PROT_WRITE | 
			PROT_READ, MAP_SHARED, image_file_fd, 0);
    if(image_buffer == (caddr_t) -1) {
      info_handler(ierr_3,NULL, "mmap error, cannot map memory to file %s.",
		   image_file);
    }
  }


  /********** allocate memory for data processing ************/

  scale = (double*) malloc(sizeof(double)*nfr);
  if(scale == NULL) {
    info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.", 
		 sizeof(double)*nfr);
  } 

  /* program comes back here if it turns out nadd is too small */
allocate_memory:


  buff_in = (double*) malloc(sizeof(double)*rng_in*(az_in+nadd));
  if(buff_in == NULL) {
    info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.", 
		 sizeof(double)*rng_in*(az_in+nadd));
  }

  array = (double*) malloc(sizeof(double)*(n_max+nadd));
  if(array == NULL) {
    info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.", 
		 sizeof(double)*(n_max+nadd));
  } 

  buff_in_rd = buff_in + rng_in*(nadd/2 + 1);

  /*
  i_preproc = 0;
  fd0_base = 5949.441361;
  fdd = 0.044304;
  fr0_base = -2088.422611;
  frd = 0.002367;
  */

  /* QDN 3/12/98  i_preproc is 2 for using the prf amb. algorithm  */
  i_preproc = 2;
  


  fd0_base_old = fd0_base;
  fdd_old = fdd;
  fr0_base_old = fr0_base;
  frd_old = frd;

  /******calculate some parameters for initial inspection*************/
  /* the parameters may not be accurate at this stage if the doppler 
     frequency used is inaccurate.
  */
  if(my_processor_number == 0) {
    lookang1 = lookang2 = lookang3 = lookang4 = lookang5 = look_angle*180.0/pi;
    frame_pp(&delta_t1,&delta_t2,&image_mean_earth_radius,
	     &nadir_mean_earth_radius,&yaw,
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
	   raw_file,image_file,&frame_mode);
    ratio = 1;
    write_info(info_file, record_length,number_of_records,ratio,
	       clutter_flag,auto_focus_flag,delta_t1,delta_t2,
	       image_mean_earth_radius,nadir_mean_earth_radius);
  }


  /********* start preprocessing (clutterlock * autofocus) *******************/
  clutter_flag = 1;
  auto_focus_flag = 0;

  if(i_preproc == 2) {

    i_dop_mode_old = i_dop_mode;
    i_dop_mode = 3;
    fac_reduce = 4;
    
    info_handler(0,NULL,"********Begin Preprocessing**********");

    set_parameters(&loopr,&loopx,&ir_st,&ix_st,&nr,&nx,&ipass,
	    &n_az_ref,&nrw,&nr_skip,&nx_skip,
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


    block_size=(loopr*loopx+number_of_processors-1)/number_of_processors;

    info_handler(0,NULL,
            "There are %d-by-%d azimuth-range blocks.Distribution block size is %d", 
            loopx, loopr, block_size); 

    for(ii = 0 ; ii <= 0 ; ii++) {
      for(jj = 0; jj < number_of_processors; jj++) {

        /* use a block distribution to select tiles for preprocessing.
           only the 1st tile in the block distribution is used */
        /*
          assigned_processor = (ii*loopx + jj)/block_size;
          if(assigned_processor == my_processor_number) {
        */

        /* using a new distribution method for preprocessing (12/31/97). */
        if(jj == my_processor_number) {
         
            i = ii+1;  /* i is range block index*/
            j = jj+1;  /* j is azimuth block index */

            info_handler(0,NULL,
                       "using azimuth-range block (%d, %d) for preprocessing",
                       j,i);

            set_parameters(&loopr,&loopx,&ir_st,&ix_st,&nr,&nx,&ipass,
                           &n_az_ref,&nrw,&nr_skip,&nx_skip,
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


            r_1st = r0_base + (i-1)*nr_skip*dr;
            r_1st= ((int)(r_1st/dr + 0.5))*dr;  /* simulates nearest integer */
            r_ref= r_1st+(ns-plen*fs)/2*dr;
            fd0 = fd0_base + fdd * (i-1)*nr_skip*dr;
            fr0 = fr0_base + frd * (i-1)*nr_skip*dr;

            /***************************************************************/
            nband = az_in*(pbw+abs(fdd)*(rng_in-plen*fs)*dr)/prf;
            nband = 2*(nband/2+1);
            info_handler(0,NULL,"nband = %d", nband);
            if(nband < az_in) nband = az_in;
            if(nband > az_in+nadd) {
              info_handler(0,NULL,
              "extra large doppler drift,increasing nadd to %d,and restarting computation.",
              nband-az_in);
              free(buff_in);
              free(array);
              nadd = nband-az_in;
              goto allocate_memory;
            }

            /* using a new distribution method for preprocessing (12/31/97).*/
            ir_st_amb = 1 + (i-1)*nr_skip;
            ix_st_amb = 1 + (j-1)*(nlines-8192)/number_of_processors;
            /* ix_st_amb = 1 ;  QDN 2/20/98 using the 1st block */

            /*****************************************************************/
            /**** right now nr_header = 0, line_offset = 0 for all cases. ****/
            /*****************************************************************/

            start_sample_amb = ir_st_amb+nr_header;
            start_line_amb   = ix_st_amb+line_offset;

            info_handler(0,NULL,"USING PRF AMB ALOGRITHM, start_sample %d start_line %d",
                    start_sample_amb,start_line_amb);

            prf_amb(&start_sample_amb,&start_line_amb,&raw_file_fd,&raw_data_offset,
                    &fac_reduce,
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
            &rs_ref,&r0_base,&fd0_base,&fr0_base,&fd0_amb,&fr0,&fdd_amb,&frd,
            &fd_const,&r_ref,&r_1st,
            &pxl_spacing,&nx_spacing,
            &t_start_frm,&t_end_frm,
            &lat1,&lon1,&rs_crn1,&rg_crn1,&lookang1,
            &lat2,&lon2,&rs_crn2,&rg_crn2,&lookang2,
            &lat3,&lon3,&rs_crn3,&rg_crn3,&lookang3,
            &lat4,&lon4,&rs_crn4,&rg_crn4,&lookang4,
            &lat5,&lon5,&rs_crn5,&rg_crn5,&lookang5,
            raw_file,image_file,
            &n_prf,&fd_pred);
            info_handler(0,NULL,"FOR PRF AMB fd0 is %f, fdd is %f",
                       fd0_amb,fdd_amb);
        }
      }
    }

    /* decide which processor's fd0_base, fdd, fr0_base, frd to use */
    fd0_base = fd0_amb;
    fdd = fdd_amb;
    strcpy(Dop_log_file,"/LOCAL/log_fd_file");
    info_handler(0,NULL,"QDN Dop_log_file is %s ",Dop_log_file);
    select_fdd_amb(fd0_base_old, fdd_old, fr0_base_old, frd_old, fd_pred,
	       number_of_processors, my_processor_number, Dop_log_file);

    i_dop_mode = i_dop_mode_old;

  } else if (i_preproc == 1){

    /* Using the energy balance algorithm */
    for(ii = 0 ; ii <= 0 ; ii++) {
      for(jj = 0; jj < number_of_processors; jj++) {

	/* use a block distribution to select tiles for preprocessing.
	   only the 1st tile in the block distribution is used */ 
	/*
	  assigned_processor = (ii*loopx + jj)/block_size;
	  if(assigned_processor == my_processor_number) {
	*/

	/* using a new distribution method for preprocessing (12/31/97). */
	if(jj == my_processor_number) {
	  
	  i = ii+1;  /* i is range block index*/
	  j = jj+1;  /* j is azimuth block index */

	  info_handler(0,NULL,
		       "using azimuth-range block (%d, %d) for preprocessing",
		       j,i); 

	  for(ijk = 1; ijk <= 2; ijk++) {     /*three iterations*/
	    set_parameters(&loopr,&loopx,&ir_st,&ix_st,&nr,&nx,&ipass,
			   &n_az_ref,&nrw,&nr_skip,&nx_skip,
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
      

	    r_1st = r0_base + (i-1)*nr_skip*dr;
	    r_1st= ((int)(r_1st/dr + 0.5))*dr;  /* simulates nearest integer */
	    r_ref= r_1st+(ns-plen*fs)/2*dr;
	    fd0 = fd0_base + fdd * (i-1)*nr_skip*dr;
	    fr0 = fr0_base + frd * (i-1)*nr_skip*dr;

	    /***************************************************************/
	    nband = az_in*(pbw+abs(fdd)*(rng_in-plen*fs)*dr)/prf;
	    nband = 2*(nband/2+1);
	    info_handler(0,NULL,"nband = %d", nband);
	    if(nband < az_in) nband = az_in;
	    if(nband > az_in+nadd) {
	      info_handler(0,NULL,"extra large doppler drift, increasing nadd to %d, and restarting computation.", nband-az_in);
	      free(buff_in);
	      free(array);
	      nadd = nband-az_in;
	      goto allocate_memory;
	    }
	  
	    /*****************************************************************/

	    /*
	    ir_st = 1 + (i-1)*nr_skip;
	    ix_st = 1 + (j-1)*nx_skip;
	    */

	    /* using a new distribution method for preprocessing (12/31/97).*/
	    ir_st = 1 + (i-1)*nr_skip;
	    ix_st = 1 + (j-1)*(nlines-az_in)/number_of_processors;

	    /*****************************************************************/
	    /**** right now nr_header = 0, line_offset = 0 for all cases. ****/
	    /*****************************************************************/
	    start_sample =ir_st+nr_header;
	    start_line = ix_st+line_offset;
	    info_handler(0,NULL,"start_sample %d start_line %d", start_sample,start_line);


	    decode2(&start_sample,&start_line,&raw_file_fd,&raw_data_offset,
                    &fac_reduce,
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
	  
            dr_preproc = dr*fac_reduce;
            fs_preproc = fs/fac_reduce;

	    if(i_squint == 0) 
	      interp2(&ir_st,&ix_st,&nadd_error,
		     &rng_in,&az_in,&nadd,&n_max,&pi,&c,&pxl_unit,
	    &ers,&jers,&rsat_s12,&rsat_s37,&rsat_ws,&rsat_fr,&rsat_eh,&rsat_el,
	    buff_in,array,buff_in_rd,
	    &mode,
	    &raw_mode,&i_dop_mode,&nband,&nlines,&line_offset,
	    &ns,&nr_header,&ns_header,&i_squint,&i_pt_sim,&i_data_type,&nlook,
	    &nfr,&nfx,&icomplex,&i_preproc,&i_iqqi,&nx_offset,&i_dummy,
	    &lambda,&fc,&bw,&dmu,&fs_preproc,&plen,&k_chirp,&dr_preproc,
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
	  
          /*Do not use the chirp scaling method for preprocessing 
	    if(i_squint == 1) 
	      chirp_lk(&ir_st,&ix_st,&nadd_error,
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
          */
	  
	    /**************************************************************/
	    /* nrs = plen*fs;		nrs is # of samples in rng ref*/
            nrs = rng_in-(ns-plen*fs)/fac_reduce; /*nrs is samples in rng ref*/
            
	    info_handler(0,NULL,"Before cluter_focus nrs=%d, nsample=%d",
                         nrs,rng_in-nrs);
	    	    
	    cluter_focus(&auto_focus_flag,buff_in_rd,&rng_in,&az_in,&nrs,
			 &pbw,&prf,&r_1st,&fd0,&fdd,&fr0,&frd,&dr_preproc,&df);

	    fd0_base = fd0 - fdd * (i-1)*nr_skip*dr;
	    fr0_base = fr0 - frd * (i-1)*nr_skip*dr;

      
	    if(nadd_error == 1) {
	      info_handler(0,NULL,"nadd error ignored in preprocessing.");
	    }

	    info_handler(0,NULL,"the estimated fd0_base is %f", fd0_base);
	    info_handler(0,NULL,"the estimated fdd is %f", fdd);
	    info_handler(0,NULL,"the estimated fr0_base is %f", fr0_base);
	    info_handler(0,NULL,"the estimated frd is %f", frd);
	  }
	  /* use only the 1st tile in block distribution for preprocessing */ 
	  goto terminate_loops;
	}
      }
    }

  terminate_loops:

    /* decide which processor's fd0_base, fdd, fr0_base, frd to use */
    select_fdd(fd0_base_old, fdd_old, fr0_base_old, frd_old, 
	       number_of_processors, my_processor_number);

    info_handler(0,NULL,
	       "BALANCE ALG: fd0_base %f, fdd %f, fr0_base %f, frd %f",
	       fd0_base, fdd, fr0_base, frd);

    info_handler(0,NULL,
	       "New Values: fd0_base %f, fdd %f, fr0_base %f, frd %f",
	       fd0_base, fdd, fr0_base, frd);

    i_dop_mode = i_dop_mode_old;
  }

  info_handler(0,NULL,
               "PREDICT VALUES: fd0_base %f, fdd %f, fr0_base %f, frd %f",
               fd0_base_old, fdd_old, fr0_base_old, frd_old);
  info_handler(0,NULL,
               "AFTER PREPROC: fd0_base %f, fdd %f, fr0_base %f, frd %f",
               fd0_base, fdd, fr0_base, frd);


  /* End of MPI program  after PRE PROCCESSOR  
  MPI_Finalize();
  if(i_preproc == 1) return 0;
  */


  /******************** end clutterlock & autofocus *******************/

  info_handler(0,NULL,"********Begin SAR processing*********");

  set_parameters(&loopr,&loopx,&ir_st,&ix_st,&nr,&nx,
	    &ipass,&n_az_ref,&nrw,&nr_skip,&nx_skip,
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

  /********* scale factors ********/
  for(i = 0; i < nfr; i++) {
    scale[i] = 1.0;
  }
  
  if(strncmp(request.comp_flag,"YES",3) == 0) {

    /* perform antenna compensation */
    if(antenna_flag == 0) {

      /* read our own atenna pattern */
      info_handler(ierr_2,NULL,"the use of self-defined antenna pattern not implemented"); 

    } else if(antenna_flag == 1) {

      info_handler(0,NULL,"reading calibration file");
      /* read antenna pattern from calibration file */
      /* build calibration file name from job id */

      sprintf(dummy, "/home/tmpdisk/calib.dat.%d", request.job_id);

      if((error_code = get_calibration(dummy,&calibration,error_msg)) != 0) {
	info_handler(error_code, NULL, "%s", error_msg);
      }


      antenna_pattern_gain = (float*)malloc(sizeof(float)*700);

      if(antenna_pattern_gain == NULL) {
	info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.",
		     sizeof(float)*700);
      }

      radio_metric_interp(antenna_pattern_gain,
			  &look_ang_cnt, &ele_ang_ref, 
			  &new_elev_incr,
			  &calibration.first_elev,
			  &calibration.elev_incr,
			  calibration.gain_vec,
			  &calibration.gain_vec_length);

    } /* else if(antenna_flag == 1) */


    /* reinitialize look angles at four corners and the image center */
    lookang1 = lookang2 = lookang3 = lookang4 = lookang5 = look_angle*180.0/pi;

    ant_gain_vect = (double*) malloc(nfr*sizeof(double));
    look_ang_vect = (double*) malloc(nfr*sizeof(double));

    if(ant_gain_vect == NULL) {
      info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.",
		   sizeof(double)*nfr);
    }

    if(look_ang_vect == NULL) {
      info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory.",
		   sizeof(double)*nfr);
    }

    info_handler(0,NULL,"before radio_metric_vector");

    radio_metric_vector(scale,
            &look_ang_cnt, &ele_ang_ref, &new_elev_incr, 
            antenna_pattern_gain,
	    ant_gain_vect, look_ang_vect, &frame_mode,
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

    info_handler(0,NULL,"after radio_metric_vector");

    if(my_processor_number == 0) {
      sprintf(dummy, "/home/tmpdisk/antenna_pattern_gain.dat.%d", request.job_id);
      ant_fd = open(dummy, O_RDWR|O_CREAT|O_TRUNC, 0644);
      if(ant_fd < 0) {
	info_handler(ierr_2, dummy, "cannot open %s", dummy);
      }     
      write(ant_fd, (char*)ant_gain_vect, nfr*sizeof(double));
      close(ant_fd);

      sprintf(dummy, "/home/tmpdisk/look_angle.dat.%d", request.job_id);
      ant_fd = open(dummy, O_RDWR|O_CREAT|O_TRUNC, 0644);
      if(ant_fd < 0) {
	info_handler(ierr_2, dummy, "cannot open %s", dummy);
      }     
      write(ant_fd, (char*)look_ang_vect, nfr*sizeof(double));
      close(ant_fd);
    }


    free(ant_gain_vect);
    free(look_ang_vect);
    free(antenna_pattern_gain);

  } /* end of compensation */

  if(my_processor_number == 0) {
    sprintf(dummy, "/home/tmpdisk/scale_factor.dat.%d", request.job_id);
    ant_fd = open(dummy, O_RDWR|O_CREAT|O_TRUNC, 0644);
    if(ant_fd < 0) {
      info_handler(ierr_2, dummy, "cannot open %s", dummy);
    }     
    write(ant_fd, (char*)scale, nfr*sizeof(double));
    close(ant_fd);
  }

  for(i = 0; i < nfr; i++) {
    scale[i] = scale[i]*pow(10,request.proc_gain/20.0)*
      (2048.0/(double)(az_in))*(2048.0/(double)(rng_in))*0.6;
  }

  /* RAMP data */
  if(icomplex == 2) {
    for(i = 0; i < nfr; i++) {
      scale[i] = scale[i] * 64.0;
    }
  }

  /* Complex data */
  if(icomplex == 1) {
    for(i = 0; i < nfr; i++) {
      scale[i] = scale[i] * 256.0;
    }
  }

  /******************** start sar processing *******************/

  /*********** determine how to distribute the image blocks **************/
  info_handler(0,NULL,"image is divided into %d-by-%d range-azimuth tiles",
	       loopr,loopx);
  /* 
     Distribute the tiles.  There are loopr*loopx tiles.  We will use a
     block distribution scheme to distribute them.  Though a cyclic 
     distribution yields a better load balance, but the maximum number of
     tiles a processor can get remains the same.  The total program execution 
     time thus is unchanged.

     For a block distribution consider the following scheme:
     The number of tiles can always be expressed as
        number_of_tiless = N*number_of_processors + remainder,
     where N and remainder are integers and remainder < number_of_processors.
     Based on this
      (N*number_of_processors + remainder + number_of_processors - 1)/
         number_of_processors
          |--- N + 0, if remainder = 0;  
	 =|
          |--- N + 1, if remainder > 0;
     Thus, the block size is N if remainder = 0, and N+1 if remainder > 0,
     which can be coded as:

     block_size=(number_of_tiles+number_of_processors-1)/number_of_processors;
     
     If number_of_tiles = 18 and number_of_processors = 4, the block size
     will be 5.  Processor 0-2 will get 5 tiles, and processor 3 will only
     get 3 tiles.

     If we use cyclic distribution, processor 0 and 1 will get 5 tiles, and
     processor 2 and 3 will get 4 tiles.
  */

  number_of_tiles = loopx*loopr;

  /* detremine which processors participates the processing */
  number_of_compu_procs = number_of_processors;
  compu_proc_number = my_processor_number;

  block_size=(number_of_tiles+number_of_compu_procs-1)/number_of_compu_procs;

  info_handler(0,NULL,"each task gets a maxiumum of %d tiles",block_size);

  counter = 0;

  if(icomplex == 1) {
    /* for complex image, loop over range is the inner loop,
     * i.e., it is range block distribution with block size equaling
     * block_size
     */
    loop_inner = loopr;
    loop_outer = loopx;
  } else {
    /* for none complex image, loop over range is the outer loop,
     * i.e., it is azimuth block distribution with block size equaling
     * block_size
     */
    loop_inner = loopx;
    loop_outer = loopr;
  }  


  /******** main loop *********/
  for(outer = 0; outer < loop_outer; outer++) {
    for(inner = 0; inner < loop_inner; inner++) {
      /*for(inner = 4; inner <= 4; inner++) {*/

      /* determine to which processor this block belongs */

      if(icomplex == 1) {
	/* for COMPLEX image, loop over range is the INNER loop */
	i = inner + 1;
	j = outer + 1;
      } else {
	/* for NON-COMPLEX image, loop over range is the OUTER loop */
	i = outer + 1;
	j = inner + 1;
      }
      /* block distribution */ 
      assigned_processor = (inner + outer*loop_inner)/block_size;

      if(assigned_processor == compu_proc_number) {
	/* process this block */
	dtime(time);

	info_handler(0,NULL,"task %d starts to work on block %d, percentage of work done %f", my_processor_number,counter+1,(double)counter*100/block_size);

	counter++;

	r_1st = r0_base + (i-1)*nr_skip*dr;
	r_1st= ((int)(r_1st/dr + 0.5))*dr;  /* simulates nearest integer */
	r_ref= r_1st+(ns-plen*fs)/2*dr;

	fd0 = fd0_base + fdd * (i-1)*nr_skip*dr;
	fr0 = fr0_base + frd * (i-1)*nr_skip*dr;
	/***********************************************************/
	nband = az_in*(pbw+abs(fdd)*(rng_in-plen*fs)*dr)/prf;
	nband = 2*(nband/2+1);
	info_handler(0,NULL,"nband = %d", nband);
	if(nband < az_in) nband = az_in;
	if(nband > az_in+nadd) {
	  info_handler(0,NULL,"extra large doppler drift, increasing nadd to %d, and restarting computation.", nband-az_in);
	  free(buff_in);
	  free(array);
	  nadd = nband-az_in;
	  i_preproc = 0;  /* no more preprocessing */
	  goto allocate_memory;
	}


	dtime(time1);

	/*************************************************************/
	ir_st = 1 + (i-1)*nr_skip;
	ix_st = 1 + (j-1)*nx_skip;
	/**************************************************************/
	/**** right now nr_header = 0, line_offset = 0 for all cases. ****/
	/*****************************************************************/
	start_sample =ir_st+nr_header;
	start_line = ix_st+line_offset;
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

	dtime(time1);
	info_handler(0,NULL,"decode processing time %f %f",
		     time1[0],time1[1]);
	
	if(i_squint == 0) 
	  interp(&ir_st,&ix_st,&nadd_error,
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

	if(i_squint == 1) 
	  chirp_lk(&ir_st,&ix_st,&nadd_error,
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

	if(nadd_error == 1) {
	  info_handler(0,NULL,"increasing nadd from %d to %d. restarting computation.", nadd, nadd*2);
	  free(buff_in);
	  free(array);
	  nadd *= 2;
	  i_preproc = 0;  /* no more preprocessing */
	  goto allocate_memory;
	}


	dtime(time1);
	info_handler(0,NULL,"interp/chirp_lk processing time %f %f",
		     time1[0],time1[1]);
	

	/***********resample and overlay*****************************/
	
	weights(w);             /*set 4pt interp weights*/

	/* 8 bit image resample and overlay */
	if(icomplex == 0) {
	  r_resample(w,&ir_st,&ix_st,scale,image_buffer,
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
	}

	/* 16 bit image resample and overlay */
	if(icomplex == 2) { 
	  r_resample_16(w,&ir_st,&ix_st,scale,(unsigned short*)image_buffer,
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
	}


	/* 32 bit image overlay */
	if(icomplex == 1) {
	  overlay_32((int*)image_buffer, ix_st, ir_st, scale);
	}


	dtime(time1);
	info_handler(0,NULL,"resampling and overlaying time %f %f",
		     time1[0],time1[1]);
	

	/* 8 bit image merge and overlay */
#ifdef MERGE_8_BIT
	if(icomplex == 0) {
	  info_handler(0,NULL,"merging 8 bit image");
	  if(loopr*loopx%number_of_processors != 0) 
	    info_handler(0,NULL,"WARNING: number of blocks not evenly dividable by number of processors.  merging image may hang.");
	  merge_image_8(my_processor_number,number_of_processors,
			image_buffer,nfr,nfx);
  	  if(my_processor_number == 0) {
	    lseek(image_file_fd, (long)0, 0);
	    write(image_file_fd,image_buffer,image_buffer_size);
          }
	  info_handler(0,NULL,"finish merging 8 bit image");
	}
#endif

	/* 16 bit image merge and overlay */
#ifdef MERGE_16_BIT
	if(icomplex == 2) {
	  info_handler(0,NULL,"merging 16 bit image");
	  if(loopr*loopx%number_of_processors != 0) 
	    info_handler(0,NULL,"WARNING: number of blocks not evenly dividable by number of processors.  merging image may hang.");
	  merge_image_16(my_processor_number,number_of_processors,
			(unsigned short*)image_buffer,nfr,nfx);
  	  if(my_processor_number == 0) {
	    lseek(image_file_fd, (long)0, 0);
	    write(image_file_fd,image_buffer,image_buffer_size);
          }
	  info_handler(0,NULL,"finish merging 16 bit image");
	}
#endif

	/* estimate the percentage of work completed.*/
	if(my_processor_number == 0) {
	  finished_blocks++;
	  if(finished_blocks != block_size) {
	    fprintf(fptr,"3 %.2lf\n",-finished_blocks/(double)block_size);
	    fflush(fptr);
	  }
	}

	dtime(time);
	info_handler(0,NULL,"one block processing time %f %f",
		     time[0],time[1]);
	

      }



    } /* for(inner = 0; inner < loop_inner; inner++) { */
  } /*   for(outer = 0; outer < loop_outer; outer++) { */

  /* calculate some information of the image for output to ceos */
  if(my_processor_number == 0) {
    /* reinitialize look angles at four corners and the image center */
    lookang1 = lookang2 = lookang3 = lookang4 = lookang5 = look_angle*180.0/pi;
    frame_pp(&delta_t1,&delta_t2,&image_mean_earth_radius,
	     &nadir_mean_earth_radius,&yaw,
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
	   raw_file,image_file,&frame_mode);
  }


  ratio = 1.0;
  new_nfr = nfr;
  new_nfx = nfx;
  /********* 8 bit image merge and overlay. write image out *********/
  if(icomplex == 0) {
    info_handler(0,NULL, "final merging 8 bit image");
    merge_image_8(my_processor_number,number_of_processors,
		  image_buffer,nfr,nfx);
    info_handler(0,NULL, "finish merging 8 bit image");
    /* pixel spacing = 100 products are generated from pixel spacing = 12.5
       images.  we average over 8x8 pixels to obtain one pixel value in the
       pixel spacing = 100 image 
    */
    if(my_processor_number == 0 && request.pixel_spacing == 100.0) {
      ratio = (int)(request.pixel_spacing/pxl_spacing);
      new_nfr = nfr/ratio;
      new_nfx = nfx/ratio;
      record_length = new_nfr;
      number_of_records = new_nfx;
      info_handler(0,NULL,"new nfr %d, nfx %d",new_nfr,new_nfx);
      /* averaged image is stored in the original image buffer */
      average_8(image_buffer,nfx,nfr,ratio);
    }
    if(my_processor_number == 0) {
      if(lseek(image_file_fd, (long)0, 0) == -1) {
	info_handler(ierr_3,NULL,"cannot lseek 0 byte in file %s.",
		     image_file);
      }
      /* perform corner turn. image is stored in azimuth rows originally.
	 after corner turn, image will be stored in range rows. gather data 
	 into buffer and then write one row in range at a time. */
      if((tmp_buffer = (char *)malloc(new_nfr*sizeof(char))) == NULL) {
	info_handler(ierr_5,NULL,"cannot allocate %d bytes of memory",new_nfr);
      }
      for(jj = 0; jj < new_nfx; jj++) {
	for(ii = 0; ii < new_nfr; ii++) {
	  tmp_buffer[ii] = image_buffer[jj+nfx*ii];
	}
	if(write(image_file_fd,tmp_buffer,new_nfr) != new_nfr) {
	  info_handler(ierr_3,NULL, "cannot write %d bytes of data",new_nfr);
	}
      }
      free(tmp_buffer);
    }
  }


  /********* 16 bit image merge and overlay. write image out *********/
  if(icomplex == 2) { 	
    info_handler(0,NULL, "final merging 16 bit image. nfr %d, nfx %d", 
		 nfr,nfx);
    merge_image_16(my_processor_number,number_of_processors,
		   (unsigned short*)image_buffer,nfr,nfx);
    info_handler(0,NULL, "finish merging 16 bit image");
    if(my_processor_number == 0) {
      if(lseek(image_file_fd, (long)0, 0) == -1) {
	info_handler(ierr_3,NULL,"cannot lseek 0 byte in file %s.",
		     image_file);
      }
      /* perform corner turn. first gather data into buffer then 
	 write one row in range at a time. */
      if((tmp_buffer = (char *)malloc(nfr*sizeof(short))) == NULL) {
	info_handler(ierr_5,NULL,"cannot allocate %d bytes of memory",
		     nfr*sizeof(short));
      }
      for(jj = 0; jj < nfx; jj++) {
	for(ii = 0; ii < nfr; ii++) {
	  ((unsigned short*)tmp_buffer)[ii] = 
	    ((unsigned short*)image_buffer)[nfx*ii+jj];
	}
	if(write(image_file_fd,tmp_buffer,nfr*sizeof(unsigned short)) !=
		 nfr*sizeof(unsigned short)){
	  info_handler(ierr_3,NULL,"cannot write %d bytes of data",
		       nfr*sizeof(short));
	}	
      }
      free(tmp_buffer);
    }
  }


  /********** 32 bit image merge and overlay *********/
  if(icomplex == 1) {
    info_handler(0,NULL, "final merging 32 bit image");
    merge_image_32(my_processor_number, number_of_processors,
		   (int*)image_buffer, record_length, number_of_records);
    info_handler(0,NULL, "finish merging 32 bit image");
  }


  /********* information for ceos file ********/
  if(my_processor_number == 0) {
    write_info(info_file, record_length,number_of_records,ratio,
	       clutter_flag,auto_focus_flag,delta_t1,delta_t2,
	       image_mean_earth_radius,nadir_mean_earth_radius);
  }

  dtime(time);
  info_handler(0,NULL,"Total cumulated user time since program started %f",
	  time[1]);

  tt2 = rtc();
  info_handler(0,NULL,"Total wall clock time since program started %lf",
	       tt2-tt1);

  /********************* end of sar processing *******************/


  if(my_processor_number == 0) {
    fprintf(fptr,"3  -1.00\n");
    fflush(fptr);
  }

  fclose(fptr);
  close(image_file_fd);
  close(raw_file_fd);

  free(buff_in);
  free(array);
  free(scale);
  free(image_buffer);

  /* End of MPI program */
  MPI_Finalize();

  return 0;
}

