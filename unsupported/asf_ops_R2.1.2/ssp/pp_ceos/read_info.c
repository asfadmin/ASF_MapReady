/* SccsId[]= @(#)read_info.c	2.41 3/24/98 */
static char sccsid_read_info[]= "@(#)PPread_info.c:2.41";

#include <stdio.h>
#include <math.h>

#include "input_file.h"
#include "error.h"

void read_info(struct IMAGE *image,
               struct DATA_SET *data_set,
               struct FACILITY_RECORD *facility_record,
               struct RANGE_SPECTRA* range_spectra,
               struct PLATFORM_POSITION* platform_position,
               int job_id)
	       
{
  FILE *fptr;
  char dummy[128];
  char file[256];
  int i;
  
  sprintf(file, "/home/tmpdisk/sps_pp_info.%d", job_id);
 
  if ((fptr = fopen(file,"r")) == NULL) 
	info_handler(ierr_2, file, "can't open file %s", file);

  fscanf(fptr, "%s",dummy);

  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%d",&image->record_length);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%d",&image->number_of_records);
  fscanf(fptr, "%s",dummy);

  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",data_set->inp_sctim);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->pro_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->pro_long);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->pro_head);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->terrain_h);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->scene_len);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->scene_wid);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->plat_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->plat_long);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->plat_head_scene);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->incident_ang);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->wave_length);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->ampl_coef0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->ampl_coef1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->ampl_coef2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->ampl_coef3);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->ampl_coef4);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->phas_coef0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->phas_coef1);
    printf("QDN in read_info.c, phas_coef1=%e \n",data_set->phas_coef1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->phas_coef2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->phas_coef3);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->phas_coef4);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%8d",&data_set->chirp_ext_ind);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->rng_samp_rate);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->rng_gate);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->rng_length);
    printf("QDN in read_info.c, rng_length=%e \n",data_set->rng_length);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->ele_sight);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->prf);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->azi_beam);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s %s",&data_set->algor_id[0], &data_set->algor_id[6]);
  data_set->algor_id[5] = ' ';
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->n_azilok);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->n_rnglok);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->bnd_azilok);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->bnd_rnglok);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->bnd_azi);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->bnd_rng);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->rng_res);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->azi_res);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->alt_dopcen0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->alt_dopcen1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->alt_dopcen2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->crt_dopcen0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->crt_dopcen1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->crt_dopcen2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->alt_rate0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->alt_rate1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->alt_rate2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->crt_rate0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->crt_rate1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->crt_rate2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",data_set->clutterlock_flg);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",data_set->auto_focus);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->line_spacing);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->pixel_spacing);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->prf1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set->rng_gate1);
  fscanf(fptr, "%s",dummy);


  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",facility_record->center_gmt);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->center_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->center_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->near_start_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->near_start_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->near_end_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->near_end_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->far_start_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->far_start_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->far_end_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->far_end_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->nadir_radius);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->image_radius);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->s_c_altitude);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->sl_rng_1st_pix);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->sl_rng_mid_pix);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->sl_rng_last_pix);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->track_angle);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->dop_frq_const);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->dop_frq_slope);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->dop_frq_quad);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->squint_angle);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->swath_velocity);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",facility_record->start_gmt);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",facility_record->end_gmt);
  fscanf(fptr, "%s",dummy);

  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->x_position);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->y_position);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->z_position);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->x_velocity);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->y_velocity);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record->z_velocity);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",dummy);

  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->x_position_begin);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->y_position_begin);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->z_position_begin);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->x_velocity_begin);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->y_velocity_begin);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->z_velocity_begin);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);

  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->x_position_middle);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->y_position_middle);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->z_position_middle);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->x_velocity_middle);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->y_velocity_middle);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->z_velocity_middle);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->x_position_end);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->y_position_end);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->z_position_end);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->x_velocity_end);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->y_velocity_end);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%lf", &platform_position->z_velocity_end);
  fscanf(fptr, "%s", dummy);
  fscanf(fptr, "%s", dummy);

  fscanf(fptr, "%lf", &platform_position->data_int);

  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",dummy);
  for (i=0; i<256; i += 4) {
    fscanf(fptr, "%lf %lf %lf %lf", &range_spectra->spectrum[i],
                                     &range_spectra->spectrum[i+1],
                                     &range_spectra->spectrum[i+2],
                                     &range_spectra->spectrum[i+3]);
  }

  fclose(fptr);

  /*  */
  printf( "    record_length = %7d\n",image->record_length);
  printf( "    number_of_records = %7d\n",image->number_of_records);
  printf( "    inp_sctim = %s\n",data_set->inp_sctim);
  printf( "    pro_lat = %16.7f\n",data_set->pro_lat);
  printf( "    pro_long = %16.7f\n",data_set->pro_long);
  printf( "    pro_head = %16.7f\n",data_set->pro_head);
  printf( "    terrain_h = %16.7f\n",data_set->terrain_h);
  printf( "    scene_len = %16.7f\n",data_set->scene_len);
  printf( "    scene_wid = %16.7f\n",data_set->scene_wid);
  printf( "    plat_lat = %8.3f\n",data_set->plat_lat);
  printf( "    plat_long = %8.3f\n",data_set->plat_long);
  printf( "    plat_head_scene = %8.3f\n",data_set->plat_head_scene);
  printf( "    incident_ang = %8.3f\n",data_set->incident_ang);
  printf( "    wave_length = %16.7f\n",data_set->wave_length);
  printf( "    ampl_coef0 = %lf\n",data_set->ampl_coef0);
  printf( "    ampl_coef1 = %lf\n",data_set->ampl_coef1);
  printf( "    ampl_coef2 = %lf\n",data_set->ampl_coef2);
  printf( "    ampl_coef3 = %lf\n",data_set->ampl_coef3);
  printf( "    ampl_coef4 = %lf\n",data_set->ampl_coef4);
  printf( "    phas_coef0 = %lf\n",data_set->phas_coef0);
  printf( "    phas_coef1 = %lf\n",data_set->phas_coef1);
  printf( "    phas_coef2 = %lf\n",data_set->phas_coef2);
  printf( "    phas_coef3 = %lf\n",data_set->phas_coef3);
  printf( "    phas_coef4 = %lf\n",data_set->phas_coef4);
  printf( "    chirp_ext_ind = %8d\n",data_set->chirp_ext_ind);
  printf( "    rng_samp_rate = %lf\n",data_set->rng_samp_rate);
  printf( "    rng_gate = %lf\n",data_set->rng_gate);
  printf( "    rng_length = %lf\n",data_set->rng_length);
  printf( "    ele_sight = %lf\n",data_set->ele_sight);
  printf( "    prf = %lf\n",data_set->prf);
  printf( "    azi_beam = %lf\n",data_set->azi_beam);
  printf( "    algor_id = %s\n",data_set->algor_id);
  printf( "    n_azilok = %lf\n",data_set->n_azilok);
  printf( "    n_rnglok = %lf\n",data_set->n_rnglok);
  printf( "    bnd_azilok = %lf\n",data_set->bnd_azilok);
  printf( "    bnd_rnglok = %lf\n",data_set->bnd_rnglok);
  printf( "    bnd_azi = %lf\n",data_set->bnd_azi);
  printf( "    bnd_rng = %lf\n",data_set->bnd_rng);
  printf( "    rng_res = %lf\n",data_set->rng_res);
  printf( "    azi_res = %lf\n",data_set->azi_res);
  printf( "    alt_dopcen0 = %lf\n",data_set->alt_dopcen0);
  printf( "    alt_dopcen1 = %lf\n",data_set->alt_dopcen1);
  printf( "    alt_dopcen2 = %lf\n",data_set->alt_dopcen2);
  printf( "    crt_dopcen0 = %lf\n",data_set->crt_dopcen0);
  printf( "    crt_dopcen1 = %lf\n",data_set->crt_dopcen1);
  printf( "    crt_dopcen2 = %lf\n",data_set->crt_dopcen2);
  printf( "    alt_rate0 = %lf\n",data_set->alt_rate0);
  printf( "    alt_rate1 = %lf\n",data_set->alt_rate1);
  printf( "    alt_rate2 = %lf\n",data_set->alt_rate2);
  printf( "    crt_rate0 = %lf\n",data_set->crt_rate0);
  printf( "    crt_rate1 = %lf\n",data_set->crt_rate1);
  printf( "    crt_rate2 = %lf\n",data_set->crt_rate2);
  printf( "    clutterlock_flg = %s\n",data_set->clutterlock_flg);
  printf( "    auto_focus = %s\n",data_set->auto_focus);
  printf( "    line_spacing = %lf\n",data_set->line_spacing);
  printf( "    pixel_spacing = %lf\n",data_set->pixel_spacing);
  printf( "    prf1 = %8.3f\n",data_set->prf1);
  printf( "    rng_gate1 = %8.3f\n",data_set->rng_gate1);


  printf( "%s    center_gmt \n",facility_record->center_gmt);
  printf( "%16.7lf    center_lat \n",facility_record->center_lat);
  printf( "%16.7lf    center_lon \n",facility_record->center_lon);
  printf( "%16.7lf    near_start_lat \n",facility_record->near_start_lat);
  printf( "%16.7lf    near_start_lon \n",facility_record->near_start_lon);
  printf( "%16.7lf    near_end_lat \n",facility_record->near_end_lat);
  printf( "%16.7lf    near_end_lon \n",facility_record->near_end_lon);
  printf( "%16.7lf    far_start_lat \n",facility_record->far_start_lat);
  printf( "%16.7lf    far_start_lon \n",facility_record->far_start_lon);
  printf( "%16.7lf    far_end_lat \n",facility_record->far_end_lat);
  printf( "%16.7lf    far_end_lon \n",facility_record->far_end_lon);

  /* */
}






