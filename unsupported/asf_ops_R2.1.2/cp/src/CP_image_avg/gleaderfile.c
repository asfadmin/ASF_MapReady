#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gleaderfile_c[] = "@(#)gleaderfile.c	1.9 97/05/20 19:02:37";

/* 
 * This is the top of the process that reads & interprets the leader file. 
 */
void
read_leader_file()
{
 
   int   leader_file_descriptor;
 
   /* open the leader file, and check it out */
   leader_file_descriptor = open(_leader_file, O_RDONLY);
 
   if (leader_file_descriptor < 0) {
       char  *open_announcement = (char *) malloc(200 * sizeof(char));
       sprintf(open_announcement, 
 	       "Cannot open the leader file %s", _leader_file);
       printfLLog(LOG_ERR, "ERROR:  %s", open_announcement);
       free(open_announcement);
       exit(QC_OPEN_LEADER_FILE_ERROR);
   }
 
   /*
    * Read data histogram record(s) and set up global variables.
    */

   interpret_data_set_summary_record(leader_file_descriptor);
   interpret_data_histogram_record(leader_file_descriptor);
 
   close(leader_file_descriptor);
   free(_leader_file); 
}


/* This reads and interprets the data set summary record, which
   is inside the leader file */
void
interpret_data_set_summary_record(leader_file_descriptor)
   int leader_file_descriptor;
{
 
   byte     data_set_summary_record_data[DATA_SET_SUMMARY_RECORD_SIZE];
   char     raw_time_string[TIME_LENGTH + 1],
            line_spacing_string[LINE_SPACING_LENGTH + 1],
            pixel_spacing_string[PIXEL_SPACING_LENGTH + 1];
   double   lat_val, long_val;
   int      number_of_bytes_read;
   char     untrue_scene_id_string[SCENE_ID_LENGTH],
            untrue_satellite_string[SATELLITE_LENGTH],
            untrue_sys_id_string[SYS_ID_LENGTH],
            untrue_revolution_string[REVOLUTION_LENGTH];
 
   /* jump to the leader file position */
   lseek(leader_file_descriptor, DATA_SET_SUMMARY_RECORD_OFFSET, 0);
 
   /* 
    * read data_set_summary_record_size bytes from leader_file_descriptor, and
    * place into data_set_summary_record_data.  Return the number of bytes
    * that were read. 
    */
   number_of_bytes_read = read(leader_file_descriptor, 
			       data_set_summary_record_data,
			       DATA_SET_SUMMARY_RECORD_SIZE);
   
   if (number_of_bytes_read != DATA_SET_SUMMARY_RECORD_SIZE) {
      char read_announcement[200];
      sprintf(read_announcement,
	      "Can't read data set summary record in leader file (%d bytes)", 
 	      DATA_SET_SUMMARY_RECORD_SIZE);
      printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);
      free(read_announcement);
      exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }

   /* 
    * validate the leader file with the content of byte 5.
    * if the vale is 10, then this id a correct leader file.
    * or report the error.
    */
   _valid_leader_file = 1;
   if (data_set_summary_record_data[LEADER_FILE_IDEN_OFFSET] 
       != LEADER_FILE_IDEN_VALUE) {
      char read_announcement[200];
      sprintf(read_announcement,
              "Problem: the leader file is invalid");
      printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);
      free(read_announcement);
      _valid_leader_file = 0;
   }
 
   /* the processing system id is at 1062-1069, 8 bytes */
   _sys_id_string = (char *) malloc(SYS_ID_LENGTH * sizeof(char));
   memmove(untrue_sys_id_string,
           &data_set_summary_record_data[SYS_ID_OFFSET],
           SYS_ID_LENGTH);

   /* Null-terminate at the first space after the first non-space */
   null_terminate_properly(untrue_sys_id_string,
                           _sys_id_string,
                           SYS_ID_LENGTH);
   _is_sys_pp = (strncmp(_sys_id_string, "PREC", 4) == 0 ? 1 : 0);
 
   /* the line spacing is at 1686-1701, 16 bytes */
   memmove(line_spacing_string, 
 	   &data_set_summary_record_data[LINE_SPACING_OFFSET], 
 	   LINE_SPACING_LENGTH);
   line_spacing_string[LINE_SPACING_LENGTH] = NULL;
 
   _line_spacing = atof(line_spacing_string);
 
   /* the pixel spacing is at 1702-1717, 16 bytes */
   memmove(pixel_spacing_string, 
 	   &data_set_summary_record_data[PIXEL_SPACING_OFFSET], 
 	   PIXEL_SPACING_LENGTH);
   pixel_spacing_string[PIXEL_SPACING_LENGTH] = NULL;
 
   _pixel_spacing = atof(pixel_spacing_string);
 
   /* the latitude is at 116-131, 16 bytes */
   _latitude_string  = (char *) malloc((LATITUDE_LENGTH + 1) * sizeof(char));
   if (!_latitude_string) {
      char open_announcement[200];
      sprintf(open_announcement,"Couldn't allocate a few bytes");
      printfLLog(LOG_ERR, "ERROR:  %s", open_announcement);
      free(open_announcement);
      exit(QC_MALLOC_ERROR);
   }
 
   memmove(_latitude_string, 
 	   &data_set_summary_record_data[LATITUDE_OFFSET], 
 	   LATITUDE_LENGTH);
   _latitude_string[LATITUDE_LENGTH] = NULL;
 
   lat_val = atof(_latitude_string);
 
   sprintf(_latitude_string, "%16.7f", lat_val);
 
   /* the longitude is at 132-147, 16 bytes */
   _longitude_string = (char *) malloc((LONGITUDE_LENGTH + 1)
 				       * sizeof(char));
   if (!_longitude_string) {
      char open_announcement[200];
      sprintf(open_announcement,"Couldn't allocate a few bytes");
      printfLLog(LOG_ERR, "ERROR:  %s", open_announcement);
      free(open_announcement);
      exit(QC_MALLOC_ERROR);
   }
 
   memmove(_longitude_string, 
 	   &data_set_summary_record_data[LONGITUDE_OFFSET], 
 	   LONGITUDE_LENGTH);
   _longitude_string[LONGITUDE_LENGTH] = NULL;
 
   long_val = atof(_longitude_string);
 
   sprintf(_longitude_string, "%16.7f", long_val);
 
   /* the time is bytes 68-99, 32 bytes */
   memmove(raw_time_string, 
 	   &data_set_summary_record_data[TIME_OFFSET], 
 	   TIME_LENGTH);
 
                                   /* add 7 for the colons */
   _time_string = (char *) malloc((TIME_LENGTH + 7)
 				  * sizeof(char));
   if (!_time_string) {
      char open_announcement[200];
      sprintf(open_announcement,"Couldn't allocate a few bytes");
      printfLLog(LOG_ERR, "ERROR:  %s", open_announcement);
      free(open_announcement);
      exit(QC_MALLOC_ERROR);
   }
 
   /* put together the time string by adding colons to the raw time string */
   add_colons_to(_time_string, raw_time_string);
 
   /* Now, get the scene_id, satellite and revolution numbers */
   _scene_id_string = (char *) malloc((SCENE_ID_LENGTH + 1) * sizeof(char));
   memmove(untrue_scene_id_string, 
 	   &data_set_summary_record_data[SCENE_ID_OFFSET],
 	   SCENE_ID_LENGTH);
 
   /* Null-terminate at the first space after the first non-space */
   null_terminate_properly(untrue_scene_id_string,
 			   _scene_id_string,
 			   SCENE_ID_LENGTH);
 
   _satellite_string = (char *) malloc(SATELLITE_LENGTH * sizeof(char));
   memmove(untrue_satellite_string, 
 	   &data_set_summary_record_data[SATELLITE_OFFSET],
 	   SATELLITE_LENGTH);
 
   /* Null-terminate at the first space after the first non-space */
   null_terminate_properly(untrue_satellite_string,
 			   _satellite_string,
 			   SATELLITE_LENGTH);
 
   _revolution_string = (char *) malloc(REVOLUTION_LENGTH * sizeof(char));
   memmove(untrue_revolution_string,
 	   &data_set_summary_record_data[REVOLUTION_OFFSET],
 	   REVOLUTION_LENGTH);
 
   /* Null-terminate at the first space after the first non-space */
   null_terminate_properly(untrue_revolution_string,
 			   _revolution_string,
 			   REVOLUTION_LENGTH);
}



/* 
 * This reads and interprets the data histogram record, which
 * is inside the leader file 
 */
void
interpret_data_histogram_record(leader_file_descriptor)
   int leader_file_descriptor;
{
 
   int   histogram_offset_amount,
         number_of_bytes_read;
   int   leader_file_descriptor_record_size 
            = LEADER_FILE_DESCRIPTOR_RECORD_SIZE;
 
   int   number_of_histogram_records,
         data_histogram_record_size,
         i_offset_q,
         i;
 
   char  *min_string, *max_string, *mean_string, *sdev_string, 
         *low_stretch_string, *high_stretch_string;
   char  *histogram_size_string, *nth_histogram_value_string,  
         *samples_per_group_string;
 
   byte  *data_histogram_record_data,
         *leader_file_descriptor_record_data;
 
   _low_pixel_stretch_value = 0;
   _high_pixel_stretch_value = 255;

   if (_valid_leader_file == 0)
      return;

   /* read in the first record (720 bytes), starting from the beginning */
   lseek(leader_file_descriptor, 0, 0);
 
   leader_file_descriptor_record_data = 
      (byte *) malloc(leader_file_descriptor_record_size * sizeof(byte));
 
   if (!leader_file_descriptor_record_data) {
      char *malloc_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(malloc_announcement, 
         "Cannot allocate %d bytes in memory for first record of leader file", 
         leader_file_descriptor_record_size);
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      free(malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }
 
   number_of_bytes_read = read(leader_file_descriptor,
 			       leader_file_descriptor_record_data,
 			       leader_file_descriptor_record_size);
 
   if (number_of_bytes_read != leader_file_descriptor_record_size) {
       char *read_announcement = (char *) malloc(200 * sizeof(char));
       sprintf(read_announcement, 
 	       "Can't read first record of leader file (%d bytes)", 
 	       leader_file_descriptor_record_size);
       printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);
       free(read_announcement);
       exit(QC_LEADER_FILE_READ_ERROR);
   }
   
   /* compute the offset from the leader file to the data histogram */
   histogram_offset_amount = 
      compute_offset_amount(leader_file_descriptor_record_data,
                            COMPUTE_HISTOGRAM_OFFSET);
 
   /* check to see how many histograms are given... */
   number_of_histogram_records = 
      read_n_bytes(NUMBER_OF_DATA_HISTOGRAM_RECORDS_LENGTH, 
                   NUMBER_OF_DATA_HISTOGRAM_RECORDS_OFFSET, 
 		   leader_file_descriptor_record_data,
 		   "number of data histogram records",
 		   CAN_BE_ZERO);
 
   if (number_of_histogram_records < 1) {
      printfLLog(LOG_WARNING, "WARNING:  %s",
	  "No data histogram records present");
      /* Set global variables to defaults, since there's no histogram record */
      return;
   }
   _number_of_histogram_records = number_of_histogram_records;
 
   /* Now, read in the histogram record. */
   /* figure out what the size of the data histogram is */
   data_histogram_record_size = 
      read_n_bytes(DATA_HISTOGRAM_RECORDS_SIZE_LENGTH, 
                   DATA_HISTOGRAM_RECORDS_SIZE_OFFSET,
 		   leader_file_descriptor_record_data,
 		   "data histogram record size",
 		   CAN_BE_ZERO);
 
   /* allocate memory for data_histogram_record. */
   data_histogram_record_data = 
      (byte *) malloc(data_histogram_record_size * sizeof(byte));
 
   if (!data_histogram_record_data) {
      char *malloc_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(malloc_announcement, 
 	      "Cannot allocate %d bytes in memory for data histogram from leader file", 
 	      data_histogram_record_size);
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      free(malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }
 
   /* go to the processed data histogram record */
   lseek(leader_file_descriptor, 
         histogram_offset_amount + 
         (number_of_histogram_records - 1) * data_histogram_record_size, 
         0);
 
   /* read in the processed data histogram */
   number_of_bytes_read = read(leader_file_descriptor,
 			       data_histogram_record_data,
 			       data_histogram_record_size);
 
   if (number_of_bytes_read != data_histogram_record_size) {
      char *read_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(read_announcement, 
              "Can't read histogram record in leader file (%d bytes)", 
              data_histogram_record_size);
      printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);
      free(read_announcement);
      exit(QC_LEADER_FILE_READ_ERROR);
   }
 
   /* Find the histogram description */
   memmove(_histogram_description, 
           &data_histogram_record_data[HIST_DESC_OFFSET],
           HIST_DESC_LENGTH);
   format_description(_histogram_description);
 
   /* Find the samples / record... */
   samples_per_group_string = (char *) malloc((SAMPLES_PER_GROUP_LENGTH + 1) 
 		              * sizeof(char));
   if (!samples_per_group_string) { 
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Memory allocation failed for samples per group");
      exit(QC_MALLOC_ERROR);
   }
   memmove(samples_per_group_string, 
 	   &data_histogram_record_data[SAMPLES_PER_GROUP_OFFSET],
 	   SAMPLES_PER_GROUP_LENGTH);
   samples_per_group_string[SAMPLES_PER_GROUP_LENGTH] = NULL;
   _samples_per_group = atoi(samples_per_group_string);
   free(samples_per_group_string);
 
   /* 
    * Find the lowest & highest sample values, and
    * set them to the low / high stretch values... 
    */
 
   /* the _low_stretch_value is bytes 132-147, 16 bytes */
   low_stretch_string = (char *) malloc((MINIMUM_SAMPLE_VALUE_LENGTH + 1)
 		        * sizeof(char));
   if (!low_stretch_string) {
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Memory allocation failed for low stretch");
      exit(QC_MALLOC_ERROR);
   }
   memmove(low_stretch_string, 
 	   &data_histogram_record_data[MINIMUM_SAMPLE_VALUE_OFFSET], 
 	   MINIMUM_SAMPLE_VALUE_LENGTH);
   low_stretch_string[MINIMUM_SAMPLE_VALUE_LENGTH] = NULL;
   _low_pixel_stretch_value = (int) atof(low_stretch_string);
   free(low_stretch_string);
   
   /* the _high_stretch_value is bytes 148-163, 16 bytes */
   high_stretch_string = (char *) malloc((MAXIMUM_SAMPLE_VALUE_LENGTH + 1)
 		         * sizeof(char));
   if (!high_stretch_string) {
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Memory allocation failed for high stretch");
      exit(QC_MALLOC_ERROR);
   }
   memmove(high_stretch_string, 
 	   &data_histogram_record_data[MAXIMUM_SAMPLE_VALUE_OFFSET], 
 	   MAXIMUM_SAMPLE_VALUE_LENGTH);
   high_stretch_string[MAXIMUM_SAMPLE_VALUE_LENGTH] = NULL;
   _high_pixel_stretch_value = (int) atof(high_stretch_string);
   free(high_stretch_string);
 
   /* Now, find the histogram max, min, and data mean, and sdev... */
   /* the mean is bytes 164-179, 16 bytes */
   mean_string = (char *) malloc((MEAN_SAMPLE_VALUE_LENGTH + 1) * sizeof(char));
   if (!mean_string) {
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Memory allocation failed for histogram mean");
      exit(QC_MALLOC_ERROR);
   }
   memmove(mean_string, 
 	   &data_histogram_record_data[MEAN_SAMPLE_VALUE_OFFSET], 
 	   MEAN_SAMPLE_VALUE_LENGTH);
   mean_string[MEAN_SAMPLE_VALUE_LENGTH] = NULL;
   _data_mean = atof(mean_string);
   free(mean_string);
 
   /* the standard deviation is bytes 180-195, 16 bytes */
   sdev_string = (char *) malloc((SDEV_SAMPLE_VALUE_LENGTH + 1) * sizeof(char));
   if (!sdev_string) {
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Memory allocation failed for histogram standard deviation");
      exit(QC_MALLOC_ERROR);
   }
   memmove(sdev_string, 
 	   &data_histogram_record_data[SDEV_SAMPLE_VALUE_OFFSET], 
 	   SDEV_SAMPLE_VALUE_LENGTH);
   sdev_string[SDEV_SAMPLE_VALUE_LENGTH] = NULL;
   _data_sdev = atof(sdev_string);
   free(sdev_string);
 
   /* the min is bytes 212-227, 16 bytes */
   min_string = (char *) malloc((MINIMUM_HISTOGRAM_VALUE_LENGTH + 1) 
 		     * sizeof(char));
   if (!min_string) {
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Memory allocation failed for histogram minimum");
      exit(QC_MALLOC_ERROR);
   }
   memmove(min_string, 
 	   &data_histogram_record_data[MINIMUM_HISTOGRAM_VALUE_OFFSET], 
 	   MINIMUM_HISTOGRAM_VALUE_LENGTH);
   min_string[MINIMUM_HISTOGRAM_VALUE_LENGTH] = NULL;
   _histogram_min = (int) atof(min_string);
   free(min_string);
 
   /* the max is bytes 228-243, 16 bytes */
   max_string = (char *) malloc((MAXIMUM_HISTOGRAM_VALUE_LENGTH + 1) * sizeof(char));
   if (!max_string) {
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Memory allocation failed for histogram maximum");
      exit(QC_MALLOC_ERROR);
   }
   memmove(max_string, 
 	   &data_histogram_record_data[MAXIMUM_HISTOGRAM_VALUE_OFFSET], 
 	   MAXIMUM_HISTOGRAM_VALUE_LENGTH);
   max_string[MAXIMUM_HISTOGRAM_VALUE_LENGTH] = NULL;
   _histogram_max = (int) atof(max_string);
   free(max_string);
 
   /* Now, set up the histogram array... */

   /* the histogram size is bytes 276 - 283, 8 bytes */
   histogram_size_string = (char *) malloc((HISTOGRAM_SIZE_LENGTH +1) * sizeof(char));
   memmove(histogram_size_string,
           &data_histogram_record_data[HISTOGRAM_SIZE_OFFSET],
           HISTOGRAM_SIZE_LENGTH);
   histogram_size_string[HISTOGRAM_SIZE_LENGTH] = NULL;
   _histogram_size = atoi(histogram_size_string);

   /* Figure out if we can handle the histogram.... */
   if (_histogram_size > 256) {
      char open_announcement[200];
      sprintf(open_announcement,"Histogram is sized over 256, ignoring histogram...");
      printfLLog(LOG_ERR, "ERROR:  %s", open_announcement);
      free(open_announcement);
   }
   else {
      /* Now assign values from the file to the histogram array */
      nth_histogram_value_string = (char *) malloc((FIRST_HISTOGRAM_VALUE_LENGTH +1) * sizeof(char));
      nth_histogram_value_string[FIRST_HISTOGRAM_VALUE_LENGTH] = NULL;

      for (i = 0; i < _histogram_size ; i++) {
         memmove(nth_histogram_value_string,
            &data_histogram_record_data[FIRST_HISTOGRAM_VALUE_OFFSET + (i*8)],
            8);
         _histogram_array[i] = atoi(nth_histogram_value_string);
      }

      /* Find the locations of the histogram max & min ... */
      _histogram_max = _histogram_min = 0;
      for (i = 0; i < (_histogram_size - 1) ; i++) {
         if (_histogram_array[i] >= _histogram_max) {
            _histogram_max = _histogram_array[i];
            _histogram_max_location = i;
         }
      }

      for (i = (_histogram_size - 1); i >= 0 ; i--) {
         if (_histogram_array[i] <= _histogram_min) {
            _histogram_min = _histogram_array[i];
            _histogram_min_location = i;
         }
      }
      /* recalculate _data_mean for SSP and PP from histogram */
      if (_is_sys_pp == 1) {
         double dsum=0, sum=0;
         if (_is_sys_ramp == 2) { /* PP COMPLEX */
            for (i=0; i<128; i++) {
               dsum += (double) _histogram_array[i]
                     * ((double) (128-i)*256-128);
               sum  += (double) _histogram_array[i];
            }
            for (i=0; i<128; i++) {
               dsum += (double) _histogram_array[128+i]
                     * ((double) (i*256+128));
               sum  += (double) _histogram_array[128+i];
            }
            _data_mean = dsum / sum;
         }
         else if (_is_sys_ramp == 1) { /* PP RAMP */
            for (i=0; i<256; i++) {
               dsum += (double) _histogram_array[i]
                     * ((double) i*256+128);
               sum  += (double) _histogram_array[i];
            }
            _data_mean = dsum / sum;
         }
      }
   }

   /*
    * All done -- no information is needed from the signal data histogram
    * record.
    */
   free(data_histogram_record_data);
   free(leader_file_descriptor_record_data);
   return;
}

/* 
 * This routine ignores all the trailing '$' and converts the rest of
 * '$' to space.
 */

void
format_description(char *hist_desc)
{
   int   i, len;

   len = strlen(hist_desc);
   for (i=len; i>0; ) {
      if (hist_desc[--i] != ' ') {
         hist_desc[++i] = '\0';
         break;
      }
   }
}


/* 
 * This routine figures out the offset where the data histogram record 
 * is inside the leader file 
 */

int
compute_offset_amount(byte_data, offset_to_compute)
   byte *byte_data;
   int  offset_to_compute;
{
 
   int   record_size            = 0,
         N_of_records           = 0,
         return_offset_amount   = 0;
 
   /* begin with the first record size */
   return_offset_amount += LEADER_FILE_DESCRIPTOR_RECORD_SIZE;
   
   /* offset #1:  Data set summary record */
   N_of_records = read_n_bytes(NUMBER_OF_DATA_SET_SUMMARY_RECORDS_LENGTH,
 			       NUMBER_OF_DATA_SET_SUMMARY_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of data set summary records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(DATA_SET_SUMMARY_RECORDS_SIZE_LENGTH, 
 		              DATA_SET_SUMMARY_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "data set summary record size",
 		              CAN_BE_ZERO);
   
   return_offset_amount += (N_of_records * record_size);
 
   /* offset #2:  Map projection data records */
   N_of_records = read_n_bytes(NUMBER_OF_MAP_PROJECTION_DATA_RECORDS_LENGTH, 
 			       NUMBER_OF_MAP_PROJECTION_DATA_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of map projection data records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(MAP_PROJECTION_RECORDS_SIZE_LENGTH,
 		              MAP_PROJECTION_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "map projection record size",
 		              CAN_BE_ZERO);
   
   return_offset_amount += (N_of_records * record_size);
 
   /* offset #3:  Platform position data records */
   N_of_records = read_n_bytes(NUMBER_OF_PLATFORM_POSITION_DATA_RECORDS_LENGTH,
 		       	       NUMBER_OF_PLATFORM_POSITION_DATA_RECORDS_OFFSET,
 			       byte_data,
 			       "number of platform position data records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(PLATFORM_POSITION_RECORDS_SIZE_LENGTH, 
 		              PLATFORM_POSITION_RECORDS_SIZE_OFFSET,
 			      byte_data,
 			      "platform position record size", 
 			      CAN_BE_ZERO);
 
   return_offset_amount += (N_of_records * record_size);
 
   /* offset #4:  Attitude record */
   N_of_records = read_n_bytes(NUMBER_OF_ATTITUDE_DATA_RECORDS_LENGTH,
 			       NUMBER_OF_ATTITUDE_DATA_RECORDS_OFFSET,
 			       byte_data,
 			       "number of attitude summary records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(ATTITUDE_RECORDS_SIZE_LENGTH,
                              ATTITUDE_RECORDS_SIZE_OFFSET,
 		              byte_data,
 			      "attitude record size",
 		              CAN_BE_ZERO);
   
   return_offset_amount += (N_of_records * record_size);
 
   /* offset #5:  Radiometric data record */
   N_of_records = read_n_bytes(NUMBER_OF_RADIOMETRIC_DATA_RECORDS_LENGTH, 
 			       NUMBER_OF_RADIOMETRIC_DATA_RECORDS_OFFSET, 
 		  	       byte_data,
 			       "number of radiometric records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(RADIOMETRIC_RECORDS_SIZE_LENGTH, 
 			      RADIOMETRIC_RECORDS_SIZE_OFFSET, 
 			      byte_data,
 			      "radiometric record size",
 			      CAN_BE_ZERO);
 
   return_offset_amount += (N_of_records * record_size);
 
   /* offset #6:  Radiometric compensation record */
   N_of_records = read_n_bytes(NUMBER_OF_RADIOMETRIC_COMPENSATION_DATA_RECORDS_LENGTH, 
 			       NUMBER_OF_RADIOMETRIC_COMPENSATION_DATA_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of radiometric compensation records",
 			       CAN_BE_ZERO);
    
   record_size = read_n_bytes(RADIOMETRIC_COMPENSATION_RECORDS_SIZE_LENGTH, 
 		              RADIOMETRIC_COMPENSATION_RECORDS_SIZE_OFFSET,
 		              byte_data,
 			      "radiometric compensation record size",
 		              CAN_BE_ZERO);
 
   return_offset_amount += (N_of_records * record_size);
 
   if (offset_to_compute == COMPUTE_DATA_QUALITY_SUMMARY_OFFSET) {
      return return_offset_amount;
   }
 
   /* offset #7:  Data quality summary record */
   N_of_records = read_n_bytes(NUMBER_OF_DATA_QUALITY_SUMMARY_RECORDS_LENGTH, 
  		               NUMBER_OF_DATA_QUALITY_SUMMARY_RECORDS_OFFSET, 
 		               byte_data,
 		               "number of data quality records",
 		               CAN_BE_ZERO);
 
   record_size = read_n_bytes(DATA_QUALITY_SUMMARY_RECORDS_SIZE_LENGTH, 
 			      DATA_QUALITY_SUMMARY_RECORDS_SIZE_OFFSET,
 			      byte_data,
 			      "data quality record size",
 			      CAN_BE_ZERO);
 
   return_offset_amount += (N_of_records * record_size);
 
   /* Finally, the data histogram record */
   N_of_records = read_n_bytes(NUMBER_OF_DATA_HISTOGRAM_RECORDS_LENGTH, 
 			       NUMBER_OF_DATA_HISTOGRAM_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of data histogram records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(DATA_HISTOGRAM_RECORDS_SIZE_LENGTH, 
 		              DATA_HISTOGRAM_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "data histogram record size",
 		              CAN_BE_ZERO);
   
   if (offset_to_compute == COMPUTE_HISTOGRAM_OFFSET) {
      return return_offset_amount;
   }
   else {
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Fatal error in compute_offset_amount");
      exit(QC_PROGRAMMER_ERROR);
   }
}


/* 
 * This function copies non-space characters into
 * a buffer, and null-terminates the fresh buffer.
 */

void
null_terminate_properly(untrue_string, final_string, string_max_length)
   char  untrue_string[];
   char  *final_string;
   int   string_max_length;
{
   int   i = 0, 
         increment_counter = 0;

   for (i = 0; i < string_max_length; i++) {
      if (untrue_string[i] != ' ') {
	 /* If the character isn't a space, copy it into the final string */
         final_string[increment_counter++] = untrue_string[i];
      }
   }

   /* Now, null-terminate the final string properly */
   final_string[increment_counter] = NULL;

}
