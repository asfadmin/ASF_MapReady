/* SccsId[]= @(#)ssp_get_aux.c	2.41 3/24/98 */
/* Lawrence H. Chen 9/20/94 

   This subroutine extract auxiliary data from disk file

*/
#define MAX_BURST 1600
#define MAX_AGC   100

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "asf.h"
/*#include "asfcommon.h"*/
/*#include "auxiliary.h"*/
#include "ssp_aux_odl.h"
#include "error.h"


int ssp_get_aux(filename,
        aux_hdr_start_burst,aux_hdr_end_burst,
        d_aux_hdr_instr_mode,aux_beam_select,aux_burst,
        d_aux_echo_valid,aux_replica_samples,d_aux_sc_time,
        aux_window_start_time,aux_window_duration,aux_replica_agc,
        aux_rx_agc,aux_sampling_rate,aux_pulse_period,
        d_aux_yaw_valid,aux_yaw_error,aux_yaw_rate,
        d_aux_roll_valid,aux_roll_error,aux_roll_rate,
        d_aux_pitch_valid,aux_pitch_error,aux_pitch_rate,
        aux_echo_samples,aux_chirp_type,aux_echo_pulses,
        aux_pulse_count_1,aux_pulse_count_2,
        aux_wdp_np,aux_wdp_chg,aux_wdp_cnt,aux_agc_np,aux_agc_chg,aux_agc_cnt,
        aux_lna_temp,aux_subsystem_temp,aux_protector_temp,
        aux_calibration_temp,aux_repl_pow,bit_error_rate)
        

char *filename;
char       d_aux_echo_valid[1600][10];
char       d_aux_sc_time[1600][40];
char       d_aux_yaw_valid[1600][10];
char       d_aux_roll_valid[1600][10];
char       d_aux_pitch_valid[1600][10];
char       d_aux_hdr_instr_mode[60];
   double*  aux_repl_pow;
   double*  bit_error_rate;
   int      *aux_hdr_start_burst;
   int      *aux_hdr_end_burst;
   int       aux_beam_select[1600];
   int       aux_burst[1600];
   int       aux_replica_samples[1600];
   double  aux_window_start_time[1600];
   double  aux_window_duration[1600];
   double  aux_replica_agc[1600];
   double  aux_rx_agc[1600];
   double  aux_sampling_rate[1600];
   double  aux_pulse_period[1600];
   double  aux_yaw_error[1600];
   double  aux_yaw_rate[1600];
   double  aux_roll_error[1600];
   double  aux_roll_rate[1600];
   double  aux_pitch_error[1600];
   double  aux_pitch_rate[1600];
        int       aux_echo_samples[1600];
        int       aux_chirp_type[1600];
        int       aux_echo_pulses[1600];
        int       aux_pulse_count_1[1600];
        int       aux_pulse_count_2[1600];

   double  aux_wdp_chg[1600][MAX_AGC];
   int     aux_wdp_np[1600][MAX_AGC];
   int     aux_agc_np[1600][MAX_AGC];
   int     aux_wdp_cnt[1600];
   double  aux_agc_chg[1600][MAX_AGC];
   int     aux_agc_cnt[1600];

   double  aux_lna_temp[1600];
   double  aux_subsystem_temp[1600];
   double  aux_protector_temp[1600];
   double  aux_calibration_temp[1600];


{
  struct stat stbuf;
  char *buf;

/*  static char	buf[1476851]l; */
  ODL msg, odl, *burst ,ack;
  int fd, nb, ierr;
  int no_record;
  char **body_key;
  int t_burst, i;
  char filename_c[60];
  extern int pass_aux_();
  int agc_np_maxcol;
  int agc_chg_maxcol;
  int agc_np_maxrow;
  int agc_chg_maxrow;
/*****************************************/
   int       aux_hdr_number_records;
   int       aux_hdr_prod_id;
   int       aux_hdr_proc_id;
   int       aux_hdr_frame_id;
   int       aux_hdr_rev;
   int       aux_hdr_seq;
   char    aux_hdr_msg_type[60];
   char    aux_hdr_source[60];
   char    aux_hdr_destination[60];
   char    aux_hdr_date[60];
   char    aux_hdr_frame_mode[60];
   char    aux_hdr_instr_mode[60];
   char    aux_hdr_satellite[60];
   char    aux_hdr_sensor[60];
   double  aux_cal1_attenuator[1600];
   double  aux_cal2_attenuator[1600];
        int       aux_sc_day[1600];
        char   aux_echo_valid[1600][10];
        char   aux_replica_valid[1600][10];
        char   aux_roll_valid[1600][10];
        char   aux_pitch_valid[1600][10];
        char   aux_yaw_valid[1600][10];
        char   aux_replica_present[1600][10];
        char   aux_sc_time[1600][40];

/*****************************************/
        char buff[300];
  str_f_to_c(filename_c,filename,60);

  agc_np_maxrow = 1;
  agc_chg_maxrow = 1;



  if ((fd = open(filename_c, 0)) == -1) {
      printf ("can't open %s\n", filename);
      sprintf(buff,"cannot open %s during parsing %s",filename_c,filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_2);
  }
  if (fstat(fd, &stbuf) == -1) {
      printf("can't stat %s\n", filename);
      sprintf(buff,"cannot open %s during parsing %s",filename_c,filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_2);
  }
  if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
      printf("can't malloc %s\n", filename);
      sprintf(buff,"cannot malloc during parsing %s",filename_c);
      printclog(3,buff);
      return(ierr_5);
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      printf("can't read %d\n", stbuf.st_size); 
      sprintf(buff,"cannot read during parsing %s",filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_4);
  }
  close(fd);

/* ODLinit() */

  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) { 
      printf("can't parse string\n");
      sprintf(buff,"cannot parse through %s",filename_c);
      printclog(3,buff);
      return(ierr_11);
  }
  free(buf); 

  strcpy(aux_hdr_msg_type, ODLGetString(msg, MSG_TYPE, &ierr));
     if (ierr != 0)
       return(ierr_12);
  strcpy(aux_hdr_destination, ODLGetString(msg, DESTINATION, &ierr));
     if (ierr != 0)
       return(ierr_12);
  strcpy(aux_hdr_source, ODLGetString(msg, SOURCE, &ierr));
     if (ierr != 0)
       return(ierr_12);
  strcpy(aux_hdr_date, ODLGetTime(msg, DATE, &ierr));
     if (ierr != 0)
       return(ierr_12);
  aux_hdr_number_records = ODLGetInt(msg, NUMBER_RECORDS, &ierr);
     if (ierr != 0)
       return(ierr_12);
  aux_hdr_proc_id = ODLGetInt(msg, PROC_ID, &ierr);
     if (ierr != 0)
       return(ierr_12);
  aux_hdr_frame_id = ODLGetInt(msg, FRAME_ID, &ierr);
     if (ierr != 0)
       return(ierr_12);
  strcpy(aux_hdr_frame_mode, ODLGetString(msg, FRAME_MODE, &ierr)); 
     if (ierr != 0)
       return(ierr_12);
  strcpy(aux_hdr_instr_mode, ODLGetString(msg, INSTRUMENT, &ierr));
     if (ierr != 0)
       return(ierr_12);
  str_c_to_f(d_aux_hdr_instr_mode,aux_hdr_instr_mode,60);
  *aux_hdr_start_burst = ODLGetInt(msg, START_BURST, &ierr);
     if (ierr != 0)
       return(ierr_12);
  *aux_hdr_end_burst = ODLGetInt(msg, END_BURST, &ierr);
     if (ierr != 0)
       return(ierr_12);
  strcpy(aux_hdr_satellite, ODLGetString(msg, SATELLITE, &ierr));
     if (ierr != 0)
       return(ierr_12);
  strcpy(aux_hdr_sensor, ODLGetString(msg, SENSOR, &ierr));
     if (ierr != 0)
       return(ierr_12);
  aux_hdr_rev = ODLGetInt(msg, REV, &ierr);
     if (ierr != 0)
       return(ierr_12);
  aux_hdr_seq = ODLGetInt(msg, SEQ, &ierr);
     if (ierr != 0)
       return(ierr_12);
   *aux_repl_pow = ODLGetDouble(msg, REPLICA_POW,&ierr);
     if (ierr != 0)
       return(ierr_12);

   *bit_error_rate = ODLGetDouble(msg, BIT_ERROR_RATE,&ierr);
     if (ierr != 0)
       return(ierr_12);

  if (! (burst = (ODL*) Val(Lookup(msg, "AUXILIARY_FILE.BODY")))) {
    return(ierr_12);
  }
  for (i = 0; (odl = burst[i]) != NULL; ++i) {
     if (strcasecmp(Name(odl), "BURST_INFO"))
	continue;

     t_burst = ODLGetInt(odl, "BURST", &ierr) - 1;
     if (ierr != 0)
       return(ierr_12);

     aux_burst[t_burst] = ODLGetInt(odl, "BURST", &ierr);
     if (ierr != 0)
       return(ierr_12);

     strcpy(&aux_echo_valid[t_burst][0],
	    ODLGetString(odl, "ECHO_VALID", &ierr));
     str_c_to_f(&d_aux_echo_valid[t_burst][0],&aux_echo_valid[t_burst][0],10);
     if (ierr != 0)
       return(ierr_12);

     aux_echo_samples[t_burst] = ODLGetInt(odl, "ECHO_SAMPLES", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_echo_pulses[t_burst] = ODLGetInt(odl, "ECHO_PULSES", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_replica_samples[t_burst] = 
       ODLGetInt(odl, "REPLICA_SAMPLES", &ierr);
     if (ierr != 0)
       return(ierr_12);

     strcpy(&aux_replica_valid[t_burst][0], 
	 ODLGetString(odl, "REPLICA_VALID", &ierr));
     if (ierr != 0)
       return(ierr_12);
     
     aux_beam_select[t_burst] = ODLGetInt(odl, "BEAM_SELECT", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_replica_agc[t_burst] = ODLGetDouble(odl, "REPLICA_AGC",&ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_cal1_attenuator[t_burst] =
	ODLGetDouble(odl, "CAL1_ATTENUATOR", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_cal2_attenuator[t_burst] = 
	ODLGetDouble(odl, "CAL2_ATTENUATOR", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_chirp_type[t_burst] = ODLGetInt(odl, "CHIRP_TYPE", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_lna_temp[t_burst] = ODLGetDouble(odl, "LNA_TEMP", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_subsystem_temp[t_burst] = 
       ODLGetDouble(odl, "SUBSYSTEM_TEMP", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_protector_temp[t_burst] = 
       ODLGetDouble(odl, "PROTECTOR_TEMP", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_calibration_temp[t_burst] = 
       ODLGetDouble(odl, "CALIBRATION_TEMP", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_sampling_rate[t_burst] = 
       ODLGetDouble(odl, "SAMPLING_RATE", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_pulse_count_1[t_burst] = 
       ODLGetInt(odl, "PULSE_COUNT_1", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_pulse_count_2[t_burst] = 
       ODLGetInt(odl, "PULSE_COUNT_2", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_pulse_period[t_burst] = 
       ODLGetDouble(odl, "PULSE_PERIOD", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_window_start_time[t_burst] = 
       ODLGetDouble(odl, "WINDOW_START_TIME", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_window_duration[t_burst] = 
       ODLGetDouble(odl, "WINDOW_DURATION", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_roll_error[t_burst] = 
       ODLGetDouble(odl, "ROLL_ERROR", &ierr);
     if (ierr != 0)
       return(ierr_12);

     strcpy(&aux_roll_valid[t_burst][0],
	    ODLGetString(odl, "ROLL_VALID",&ierr));
     str_c_to_f(&d_aux_roll_valid[t_burst][0],&aux_roll_valid[t_burst][0],10);
     if (ierr != 0)
       return(ierr_12);

     aux_pitch_error[t_burst] = ODLGetDouble(odl, "PITCH_ERROR",&ierr);
     if (ierr != 0)
       return(ierr_12);

     strcpy(&aux_pitch_valid[t_burst][0],
	    ODLGetString(odl, "PITCH_VALID", &ierr));
     str_c_to_f(&d_aux_pitch_valid[t_burst][0],&aux_pitch_valid[t_burst][0],10);
     if (ierr != 0)
       return(ierr_12);

     aux_yaw_error[t_burst] = ODLGetDouble(odl, "YAW_ERROR", &ierr);
     if (ierr != 0)
       return(ierr_12);

     strcpy(&aux_yaw_valid[t_burst][0], 
	    ODLGetString(odl, "YAW_VALID", &ierr));
     str_c_to_f(&d_aux_yaw_valid[t_burst][0],&aux_yaw_valid[t_burst][0],10);
     if (ierr != 0)
       return(ierr_12);

     aux_roll_rate[t_burst] = ODLGetDouble(odl, "ROLL_RATE", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_pitch_rate[t_burst] = ODLGetDouble(odl, "PITCH_RATE", &ierr);
     if (ierr != 0)
       return(ierr_12);

     aux_yaw_rate[t_burst] = ODLGetDouble(odl, "YAW_RATE", &ierr);
     if (ierr != 0)
       return(ierr_12);

     strcpy(&aux_sc_time[t_burst][0], ODLGetTime(odl,"SC_TIME",&ierr));
     str_c_to_f(&d_aux_sc_time[t_burst][0],&aux_sc_time[t_burst][0],10);
     if (ierr != 0)
       return(ierr_12);

     strcpy(&aux_replica_present[t_burst][0], 
	    ODLGetString(odl, "REPLICA_PRESENT", &ierr));
     if (ierr != 0)
       return(ierr_12);

     aux_rx_agc[t_burst] = ODLGetDouble(odl, "RX_AGC", &ierr);
     if (ierr != 0)
       return(ierr_12);
/*************NEW***************/

     agc_np_maxcol = MAX_AGC;
     if (! ODLGetArrayInt(odl, "WINDOW_POS_CHANGES",
          &aux_wdp_np[t_burst][0], &agc_np_maxrow,&agc_np_maxcol))
       {
         printf("after %d %d\n",agc_np_maxrow,agc_np_maxcol);
         printf("cant get WINDOW_POS_CHANGES at burst # %d\n",t_burst);
         return(ierr_12);
       }

     agc_np_maxcol = MAX_AGC;
     if (! ODLGetArrayDouble(odl, "WINDOW_POS_VALUES",
          &aux_wdp_chg[t_burst][0], &agc_np_maxrow,&agc_np_maxcol))
       {
        printf("after %d %d\n",agc_np_maxrow,agc_np_maxcol);
        printf("cant get WINDOW_POS_VALUES at burst # %d\n",t_burst);
        return(ierr_12);
       }
     aux_wdp_cnt[t_burst]=agc_np_maxcol;

     agc_np_maxcol = MAX_AGC;
     if (! ODLGetArrayInt(odl, "AGC_CHANGES",
          &aux_agc_np[t_burst][0], &agc_np_maxrow,&agc_np_maxcol))
       {
        printf("after %d %d\n",agc_np_maxrow,agc_np_maxcol);
        printf("cant get AGC_CHANGES at burst # %d\n",t_burst);
        return(ierr_12);
       }

     agc_np_maxcol = MAX_AGC;
     if (! ODLGetArrayDouble(odl, "AGC_VALUES",
           aux_agc_chg[t_burst], &agc_np_maxrow, &agc_np_maxcol))
       {
        printf("after %d %d\n",agc_np_maxrow,agc_np_maxcol);
        printf("cant get AGC_VALUES at burst# %d \n",t_burst);
        return(ierr_12);
       }
     aux_agc_cnt[t_burst]=agc_np_maxcol;


     
  }
 

  ODLFree(msg);
  return(0);
}
