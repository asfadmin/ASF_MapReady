/* SccsId[]= @(#)read_info.c	2.41 3/24/98 */
static char sccsid_read_info[]= "@(#)PPread_info.c:2.41";

#include <stdio.h>
#include <math.h>
#include "input_file.h"

void read_info(struct IMAGE image,
	       struct DATA_SET data_set,
	       struct FACILITY_RECORD facility_record)
	       
{
  FILE *fptr;
  char dummy[128];

  fptr = fopen("sps_pp_info","r");

  fscanf(fptr, "%s",dummy);

  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%d",&image.record_length);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%d",&image.number_of_records);
  fscanf(fptr, "%s",dummy);

  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",data_set.inp_sctim);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.pro_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.pro_long);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.pro_head);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.terrain_h);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.scene_len);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.scene_wid);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.plat_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.plat_long);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.plat_head_scene);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.incident_ang);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.wave_length);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.ampl_coef0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.ampl_coef1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.ampl_coef2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.ampl_coef3);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.ampl_coef4);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.phas_coef0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.phas_coef1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.phas_coef2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.phas_coef3);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.phas_coef4);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%8d",&data_set.chirp_ext_ind);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.rng_samp_rate);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.rng_gate);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.rng_length);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.ele_sight);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.prf);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.azi_beam);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s %s",&data_set.algor_id[0], &data_set.algor_id[6]);
  data_set.algor_id[5] = ' ';
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.n_azilok);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.n_rnglok);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.bnd_azilok);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.bnd_rnglok);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.bnd_azi);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.bnd_rng);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.rng_res);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.azi_res);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.alt_dopcen0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.alt_dopcen1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.alt_dopcen2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.crt_dopcen0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.crt_dopcen1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.crt_dopcen2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.alt_rate0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.alt_rate1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.alt_rate2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.crt_rate0);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.crt_rate1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.crt_rate2);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",data_set.clutterlock_flg);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",data_set.auto_focus);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.line_spacing);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.pixel_spacing);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.prf1);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&data_set.rng_gate1);
  fscanf(fptr, "%s",dummy);


  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%s",facility_record.center_gmt);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.center_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.center_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.near_start_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.near_start_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.near_end_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.near_end_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.far_start_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.far_start_lon);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.far_end_lat);
  fscanf(fptr, "%s",dummy);
  fscanf(fptr, "%lf",&facility_record.far_end_lon);
  fscanf(fptr, "%s",dummy);

  fclose(fptr);

  printf( "    record_length = %7d\n",image.record_length);
  printf( "    number_of_records = %7d\n",image.number_of_records);
  printf( "    inp_sctim = %s\n",data_set.inp_sctim);
  printf( "    pro_lat = %16.7f\n",data_set.pro_lat);
  printf( "    pro_long = %16.7f\n",data_set.pro_long);
  printf( "    pro_head = %16.7f\n",data_set.pro_head);
  printf( "    terrain_h = %16.7f\n",data_set.terrain_h);
  printf( "    scene_len = %16.7f\n",data_set.scene_len);
  printf( "    scene_wid = %16.7f\n",data_set.scene_wid);
  printf( "    plat_lat = %8.3f\n",data_set.plat_lat);
  printf( "    plat_long = %8.3f\n",data_set.plat_long);
  printf( "    plat_head_scene = %8.3f\n",data_set.plat_head_scene);
  printf( "    incident_ang = %8.3f\n",data_set.incident_ang);
  printf( "    wave_length = %16.7f\n",data_set.wave_length);
  printf( "    ampl_coef0 = %16.7e\n",data_set.ampl_coef0);
  printf( "    ampl_coef1 = %16.7e\n",data_set.ampl_coef1);
  printf( "    ampl_coef2 = %16.7e\n",data_set.ampl_coef2);
  printf( "    ampl_coef3 = %16.7e\n",data_set.ampl_coef3);
  printf( "    ampl_coef4 = %16.7e\n",data_set.ampl_coef4);
  printf( "    phas_coef0 = %16.7e\n",data_set.phas_coef0);
  printf( "    phas_coef1 = %16.7e\n",data_set.phas_coef1);
  printf( "    phas_coef2 = %16.7e\n",data_set.phas_coef2);
  printf( "    phas_coef3 = %16.7e\n",data_set.phas_coef3);
  printf( "    phas_coef4 = %16.7e\n",data_set.phas_coef4);
  printf( "    chirp_ext_ind = %8d\n",data_set.chirp_ext_ind);
  printf( "    rng_samp_rate = %16.7f\n",data_set.rng_samp_rate);
  printf( "    rng_gate = %16.7f\n",data_set.rng_gate);
  printf( "    rng_length = %16.7f\n",data_set.rng_length);
  printf( "    ele_sight = %16.7f\n",data_set.ele_sight);
  printf( "    prf = %16.7f\n",data_set.prf);
  printf( "    azi_beam = %16.7f\n",data_set.azi_beam);
  printf( "    algor_id = %s\n",data_set.algor_id);
  printf( "    n_azilok = %16.7f\n",data_set.n_azilok);
  printf( "    n_rnglok = %16.7f\n",data_set.n_rnglok);
  printf( "    bnd_azilok = %16.7f\n",data_set.bnd_azilok);
  printf( "    bnd_rnglok = %16.7f\n",data_set.bnd_rnglok);
  printf( "    bnd_azi = %16.7f\n",data_set.bnd_azi);
  printf( "    bnd_rng = %16.7f\n",data_set.bnd_rng);
  printf( "    rng_res = %16.7f\n",data_set.rng_res);
  printf( "    azi_res = %16.7f\n",data_set.azi_res);
  printf( "    alt_dopcen0 = %16.7e\n",data_set.alt_dopcen0);
  printf( "    alt_dopcen1 = %16.7e\n",data_set.alt_dopcen1);
  printf( "    alt_dopcen2 = %16.7e\n",data_set.alt_dopcen2);
  printf( "    crt_dopcen0 = %16.7e\n",data_set.crt_dopcen0);
  printf( "    crt_dopcen1 = %16.7e\n",data_set.crt_dopcen1);
  printf( "    crt_dopcen2 = %16.7e\n",data_set.crt_dopcen2);
  printf( "    alt_rate0 = %16.7e\n",data_set.alt_rate0);
  printf( "    alt_rate1 = %16.7e\n",data_set.alt_rate1);
  printf( "    alt_rate2 = %16.7e\n",data_set.alt_rate2);
  printf( "    crt_rate0 = %16.7e\n",data_set.crt_rate0);
  printf( "    crt_rate1 = %16.7e\n",data_set.crt_rate1);
  printf( "    crt_rate2 = %16.7e\n",data_set.crt_rate2);
  printf( "    clutterlock_flg = %s\n",data_set.clutterlock_flg);
  printf( "    auto_focus = %s\n",data_set.auto_focus);
  printf( "    line_spacing = %16.7f\n",data_set.line_spacing);
  printf( "    pixel_spacing = %16.7f\n",data_set.pixel_spacing);
  printf( "    prf1 = %8.3f\n",data_set.prf1);
  printf( "    rng_gate1 = %8.3f\n",data_set.rng_gate1);

  printf( "%s    center_gmt \n",facility_record.center_gmt);
  printf( "%16.7lf    center_lat \n",facility_record.center_lat);
  printf( "%16.7lf    center_lon \n",facility_record.center_lon);
  printf( "%16.7lf    near_start_lat \n",facility_record.near_start_lat);
  printf( "%16.7lf    near_start_lon \n",facility_record.near_start_lon);
  printf( "%16.7lf    near_end_lat \n",facility_record.near_end_lat);
  printf( "%16.7lf    near_end_lon \n",facility_record.near_end_lon);
  printf( "%16.7lf    far_start_lat \n",facility_record.far_start_lat);
  printf( "%16.7lf    far_start_lon \n",facility_record.far_start_lon);
  printf( "%16.7lf    far_end_lat \n",facility_record.far_end_lat);
  printf( "%16.7lf    far_end_lon \n",facility_record.far_end_lon);

}

void main() 
{
  struct IMAGE image;
  struct DATA_SET data_set;
  struct FACILITY_RECORD facility_record;
  read_info(image, data_set, facility_record);
}
