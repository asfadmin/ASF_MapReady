#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qleaderfile_c[] = "@(#)qleaderfile.c	1.31 97/06/12 17:09:26";

/* 
 * This is the top of the process that reads & interprets the leader file. 
 */
void
read_leader_file()
{
 
   int   leader_file_descriptor;
   int   map_proj_exists = 0 ;
   

   /* open the leader file, and check it out */
   leader_file_descriptor = open(_leader_file, O_RDONLY);
 
   if (leader_file_descriptor < 0) {
       char  open_announcement[200];
       sprintf(open_announcement, 
 	       "Cannot open the leader file %s", _leader_file);
       make_announcement(open_announcement, LOG);
       exit(QC_OPEN_LEADER_FILE_ERROR);
   }
 
   /* reads leader file records, and sets up a global variables. */
 
   interpret_data_set_summary_record(leader_file_descriptor); 
   interpret_map_projection_data_record(leader_file_descriptor); 
   interpret_data_summary_quality_record(leader_file_descriptor);
   interpret_signal_data_histogram_record(leader_file_descriptor);
   interpret_processed_data_histogram_record(leader_file_descriptor);
 
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
      make_announcement(read_announcement, LOG);
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
      make_announcement("Problem: the leader file is invalid", LOG);
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
      make_announcement("Couldn't allocate a few bytes", LOG);
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
      make_announcement("Couldn't allocate a few bytes", LOG);
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
      make_announcement("Couldn't allocate a few bytes", LOG);
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


/* This reads and interprets the map projection data record, which is
   inside the leader file */
void
interpret_map_projection_data_record(leader_file_descriptor)
     int leader_file_descriptor;
{
 
  char     orig_projection_string[MAP_DESC_LENGTH + 2];

  byte     map_projection_data_record_data[MAP_PROJECTION_DATA_RECORD_SIZE],
           leader_file_descriptor_record_data[LEADER_FILE_DESCRIPTOR_RECORD_SIZE];
  int      number_of_bytes_read,
           map_projection_record_offset_amount,
           number_of_map_projection_records,
           map_projection_record_size,
           i;
  
   /* Get back to the start of the leader file... */
   lseek(leader_file_descriptor, 0, 0);
 
   /* Do read, leaving the information in leader_file_descriptor_record_data */
   number_of_bytes_read = read(leader_file_descriptor,
 			       leader_file_descriptor_record_data,
 			       LEADER_FILE_DESCRIPTOR_RECORD_SIZE);
 
   if (number_of_bytes_read != LEADER_FILE_DESCRIPTOR_RECORD_SIZE) {
      char read_announcement[200];
      sprintf(read_announcement, 
              "Can't read first record of leader file (%d bytes)", 
              LEADER_FILE_DESCRIPTOR_RECORD_SIZE);
      make_announcement(read_announcement, LOG);
      exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }
   
   /* Now, compute how far you have to jump in the leader file to get
      to the map projection record */
   map_projection_record_offset_amount =
     compute_offset_amount(leader_file_descriptor_record_data,
			   COMPUTE_MAP_PROJECTION_OFFSET);
   
   /* Check to see how many map projection records are given.. */
   number_of_map_projection_records = 
     read_n_bytes(NUMBER_OF_MAP_PROJECTION_RECORDS_LENGTH, 
		  NUMBER_OF_MAP_PROJECTION_RECORDS_OFFSET, 
		  leader_file_descriptor_record_data,
		  "number of map projection records",
		  CAN_BE_ZERO);
   
   if (number_of_map_projection_records < 1) {
     get_projection_from_fdr (leader_file_descriptor_record_data,
			      leader_file_descriptor);
     return ;
   } 
 
   /* Now, read in the data quality summary record... */
   /* figure out how big the record is... */
   map_projection_record_size = 
     read_n_bytes(MAP_PROJECTION_RECORDS_SIZE_LENGTH, 
 		  MAP_PROJECTION_RECORDS_SIZE_OFFSET,
 		  leader_file_descriptor_record_data,
 		  "map projection record size",
 		  CAN_BE_ZERO);
 
   /* jump to the map_projection_record */
   lseek(leader_file_descriptor, map_projection_record_offset_amount, 0);
 
   /* 
    * read map_projection_data_record_size bytes from leader_file_descriptor, and
    * place into map_projection_data_record_data.  Return the number of bytes
    * that were read. 
    */
   number_of_bytes_read = read(leader_file_descriptor, 
			       map_projection_data_record_data, 
			       MAP_PROJECTION_DATA_RECORD_SIZE);
   
   if (number_of_bytes_read != MAP_PROJECTION_DATA_RECORD_SIZE) {
      char read_announcement[200];
      sprintf(read_announcement,
	      "Can't read map projection record in leader file (%d bytes)", 
 	      MAP_PROJECTION_DATA_RECORD_SIZE);
      make_announcement(read_announcement, LOG);
      exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }

   /* the map_desc is bytes 28-59, 32 bytes */
   _projection_string = (char *) malloc((MAP_DESC_LENGTH + 1) * sizeof(char));
   if (!_projection_string) {
      make_announcement("Couldn't allocate a few bytes", LOG);
      exit(QC_MALLOC_ERROR);
   }

   memmove(orig_projection_string, 
 	   &map_projection_data_record_data[MAP_DESC_OFFSET], 
 	   MAP_DESC_LENGTH);


   null_terminate_properly (orig_projection_string,
			    _projection_string,
			    MAP_DESC_LENGTH);

   /* Check and see if the string matches pre-defined types */
   if ((strncmp(_projection_string,
		QC_UTM_STRING,
		sizeof (QC_UTM_STRING) - 1) != 0) &&
       (strncmp(_projection_string,
		QC_GROUND_RANGE_STRING,
		sizeof (QC_GROUND_RANGE_STRING) - 1) != 0) &&
       (strncmp(_projection_string,
		QC_SLANT_RANGE_STRING,
		sizeof (QC_SLANT_RANGE_STRING) - 1) != 0)  &&
       (strncmp(_projection_string,
		QC_LAMBERT_STRING,
		sizeof (QC_LAMBERT_STRING) - 1) != 0)      &&
       (strncmp(_projection_string,
		QC_PS_STRING,
		sizeof (QC_PS_STRING) - 1) != 0))
     {
       strcpy (_projection_string, "UNKNOWN");
     }

}

/* 
 * This reads and interprets the data set summary record, which
 * is inside the leader file 
 */
void
interpret_data_summary_quality_record(leader_file_descriptor)
   int leader_file_descriptor;
{
 
   int   data_quality_summary_record_offset_amount,
         number_of_bytes_read;
 
   int   number_of_data_quality_summary_records,
         data_quality_summary_record_size;
 
   byte  *data_quality_summary_record_data,
         leader_file_descriptor_record_data[LEADER_FILE_DESCRIPTOR_RECORD_SIZE];
 
   char  ber_string[BER_LENGTH + 1], snr_string[SNR_LENGTH + 1];
 
   /* Get back to the start of the leader file... */
   lseek(leader_file_descriptor, 0, 0);
 
   /* Do read, leaving the information in leader_file_descriptor_record_data */
 
   number_of_bytes_read = read(leader_file_descriptor,
 			       leader_file_descriptor_record_data,
 			       LEADER_FILE_DESCRIPTOR_RECORD_SIZE);
 
   if (number_of_bytes_read != LEADER_FILE_DESCRIPTOR_RECORD_SIZE) {
      char read_announcement[200];
      sprintf(read_announcement, 
              "Can't read first record of leader file (%d bytes)", 
              LEADER_FILE_DESCRIPTOR_RECORD_SIZE);
      make_announcement(read_announcement, LOG);
      exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }
   
   /* Now, compute how far you have to jump in the leader file 
      to get to the data histogram */
 
   data_quality_summary_record_offset_amount =
      compute_offset_amount(leader_file_descriptor_record_data,
                            COMPUTE_DATA_QUALITY_SUMMARY_OFFSET);
 
   /* Check to see how many data quality summary records are given.. */
   number_of_data_quality_summary_records = 
      read_n_bytes(NUMBER_OF_DATA_QUALITY_SUMMARY_RECORDS_LENGTH, 
                   NUMBER_OF_DATA_QUALITY_SUMMARY_RECORDS_OFFSET, 
 		   leader_file_descriptor_record_data,
 		   "number of data quality summary records",
 		   CAN_BE_ZERO);
 
   if (number_of_data_quality_summary_records < 1) {
      make_announcement("Problem: there are fewer than 1 data quality summary records", LOG);
     /* 
      * Set up global variables to defaults, since
      * there's no data quality summary record... 
      */
      _snr = 0;
      _ber = 0;
      return;
   }
 
   /* Now, read in the data quality summary record... */
   /* figure out how big the record is... */
   data_quality_summary_record_size = 
     read_n_bytes(DATA_QUALITY_SUMMARY_RECORDS_SIZE_LENGTH, 
 		  DATA_QUALITY_SUMMARY_RECORDS_SIZE_OFFSET,
 		  leader_file_descriptor_record_data,
 		  "data quality summary record size",
 		  CAN_BE_ZERO);
 
   data_quality_summary_record_data = 
      (byte *) malloc(data_quality_summary_record_size * sizeof(byte));
 
   if (!data_quality_summary_record_data) {
      char malloc_announcement[200];
      sprintf(malloc_announcement, 
	      "Cannot allocate %d bytes in memory for data quality summary from leader file", 
	      data_quality_summary_record_size);
      make_announcement(malloc_announcement, LOG);
      exit(QC_MALLOC_ERROR);
   }
 
   /* jump to the data_quality_summary_record */
   lseek(leader_file_descriptor, data_quality_summary_record_offset_amount, 0);
   number_of_bytes_read = read(leader_file_descriptor,
 			       data_quality_summary_record_data,
 			       data_quality_summary_record_size);
 
   if (number_of_bytes_read != data_quality_summary_record_size) {
      char read_announcement[200];
      sprintf(read_announcement,
	      "Can't read data quality summary record in  leader file (%d bytes)",
	      data_quality_summary_record_size);
      make_announcement(read_announcement, LOG);
      exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }
 
   /* get the signal-to-noise ratio */
   memmove(snr_string, &data_quality_summary_record_data[SNR_OFFSET],
 	   SNR_LENGTH);
   snr_string[SNR_LENGTH] = NULL;
   _snr = atof(snr_string);
 
   /* get the bit error rate */
   memmove(ber_string, &data_quality_summary_record_data[BER_OFFSET],
 	   BER_LENGTH);
   ber_string[BER_LENGTH] = NULL;
   _ber = atof(ber_string);

   free (data_quality_summary_record_data);
}


/* 
 * This reads and interprets the data histogram record, which
 * is inside the leader file 
 */
void
interpret_processed_data_histogram_record(leader_file_descriptor)
   int leader_file_descriptor;
{
 
   int   histogram_offset_amount,
         number_of_bytes_read;
 
   int   data_histogram_record_size,
         i;
 
   char  min_string[MINIMUM_HISTOGRAM_VALUE_LENGTH + 1],
         max_string[MAXIMUM_HISTOGRAM_VALUE_LENGTH + 1],
         mean_string[MEAN_SAMPLE_VALUE_LENGTH + 1],
         sdev_string[SDEV_SAMPLE_VALUE_LENGTH + 1], 
         low_stretch_string[MINIMUM_SAMPLE_VALUE_LENGTH + 1],
         high_stretch_string[MAXIMUM_SAMPLE_VALUE_LENGTH + 1];

   char  histogram_size_string[HISTOGRAM_SIZE_LENGTH + 1],
         nth_histogram_value_string[FIRST_HISTOGRAM_VALUE_LENGTH + 1],  
         samples_per_group_string[SAMPLES_PER_GROUP_LENGTH + 1];
 
   byte  *data_histogram_record_data,
         leader_file_descriptor_record_data[LEADER_FILE_DESCRIPTOR_RECORD_SIZE];
 
   _low_pixel_stretch_value = 0;
   _high_pixel_stretch_value = 255;

   if (_valid_leader_file == 0)
      return;

   /* read in the first record (720 bytes), starting from the beginning */
   lseek(leader_file_descriptor, 0, 0);
 
   number_of_bytes_read = read(leader_file_descriptor,
 			       leader_file_descriptor_record_data,
 			       LEADER_FILE_DESCRIPTOR_RECORD_SIZE);
 
   if (number_of_bytes_read != LEADER_FILE_DESCRIPTOR_RECORD_SIZE) {
       char read_announcement[200];
       sprintf(read_announcement, 
 	       "Can't read first record of leader file (%d bytes)", 
 	       LEADER_FILE_DESCRIPTOR_RECORD_SIZE);
       make_announcement(read_announcement, LOG);
       exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }
   
   /* compute the offset from the leader file to the data histogram */
   histogram_offset_amount = 
      compute_offset_amount(leader_file_descriptor_record_data,
                            COMPUTE_HISTOGRAM_OFFSET);
 
   /* check to see how many histograms are given... */
   _number_of_histogram_records = 
     read_n_bytes(NUMBER_OF_DATA_HISTOGRAM_RECORDS_LENGTH, 
		  NUMBER_OF_DATA_HISTOGRAM_RECORDS_OFFSET, 
 		   leader_file_descriptor_record_data,
		  "number of data histogram records",
		  CAN_BE_ZERO);
 
   if (_number_of_histogram_records < 1) {
      make_announcement("Problem: there are fewer than 1 data histogram records", LOG);
      /* Set global variables to defaults, since there's no histogram record */
      return;
   }
 
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
      make_announcement("Can't malloc.", LOG);
      exit(QC_MALLOC_ERROR);
   }
 
   /* go to the processed data histogram record */
   lseek(leader_file_descriptor, 
         histogram_offset_amount + 
	 /* The first record will always be the processed data */
	 (_number_of_histogram_records - 1) * data_histogram_record_size,
         0);
 
   /* read in the processed data histogram */
   number_of_bytes_read = read(leader_file_descriptor,
 			       data_histogram_record_data,
 			       data_histogram_record_size);
 
   if (number_of_bytes_read != data_histogram_record_size) {
      char read_announcement[200];
      sprintf(read_announcement, 
              "Can't read histogram record in leader file (%d bytes)", 
              data_histogram_record_size);
      make_announcement(read_announcement, LOG);
      free(data_histogram_record_data);
      return ;
   }
 
   /* Find the histogram description */
   memmove(_histogram_description, 
           &data_histogram_record_data[HIST_DESC_OFFSET],
           HIST_DESC_LENGTH);
   format_description(_histogram_description);
 
   /* Find the samples / record... */
   memmove(samples_per_group_string, 
 	   &data_histogram_record_data[SAMPLES_PER_GROUP_OFFSET],
 	   SAMPLES_PER_GROUP_LENGTH);
   samples_per_group_string[SAMPLES_PER_GROUP_LENGTH] = NULL;
   _samples_per_group = atoi(samples_per_group_string);
 
   /* 
    * Find the lowest & highest sample values, and
    * set them to the low / high stretch values... 
    */
 
   /* the _low_stretch_value is bytes 132-147, 16 bytes */
   memmove(low_stretch_string, 
 	   &data_histogram_record_data[MINIMUM_SAMPLE_VALUE_OFFSET], 
 	   MINIMUM_SAMPLE_VALUE_LENGTH);
   low_stretch_string[MINIMUM_SAMPLE_VALUE_LENGTH] = NULL;
   _low_pixel_stretch_value = (int) atof(low_stretch_string);
   if (_low_pixel_stretch_value > 255) {
      _low_pixel_stretch_value /= 256;
   }
   if (_low_pixel_stretch_value < 0) _low_pixel_stretch_value = 0;
   
   /* the _high_stretch_value is bytes 148-163, 16 bytes */
   memmove(high_stretch_string, 
 	   &data_histogram_record_data[MAXIMUM_SAMPLE_VALUE_OFFSET], 
 	   MAXIMUM_SAMPLE_VALUE_LENGTH);
   high_stretch_string[MAXIMUM_SAMPLE_VALUE_LENGTH] = NULL;
   _high_pixel_stretch_value = (int) atof(high_stretch_string);
   if (_high_pixel_stretch_value > 255) {
      _high_pixel_stretch_value /= 256;
   }
   if (_high_pixel_stretch_value < 0) _high_pixel_stretch_value = 0;
   if (_high_pixel_stretch_value < _low_pixel_stretch_value) {
       int tmp_pixel_stretch_value;
       tmp_pixel_stretch_value  = _high_pixel_stretch_value;
       _high_pixel_stretch_value = _low_pixel_stretch_value;
       _low_pixel_stretch_value  = tmp_pixel_stretch_value;
   }
 
   /* Now, find the histogram max, min, and data mean, and sdev... */
   /* the mean is bytes 164-179, 16 bytes */
   memmove(mean_string, 
 	   &data_histogram_record_data[MEAN_SAMPLE_VALUE_OFFSET], 
 	   MEAN_SAMPLE_VALUE_LENGTH);
   mean_string[MEAN_SAMPLE_VALUE_LENGTH] = NULL;
   _data_mean = atof(mean_string);
 
   /* the standard deviation is bytes 180-195, 16 bytes */
   memmove(sdev_string, 
 	   &data_histogram_record_data[SDEV_SAMPLE_VALUE_OFFSET], 
 	   SDEV_SAMPLE_VALUE_LENGTH);
   sdev_string[SDEV_SAMPLE_VALUE_LENGTH] = NULL;
   _data_sdev = atof(sdev_string);
 
   /* the min is bytes 212-227, 16 bytes */
   memmove(min_string, 
 	   &data_histogram_record_data[MINIMUM_HISTOGRAM_VALUE_OFFSET], 
 	   MINIMUM_HISTOGRAM_VALUE_LENGTH);
   min_string[MINIMUM_HISTOGRAM_VALUE_LENGTH] = NULL;
   _histogram_min = (int) atof(min_string);
 
   /* the max is bytes 228-243, 16 bytes */
   memmove(max_string, 
 	   &data_histogram_record_data[MAXIMUM_HISTOGRAM_VALUE_OFFSET], 
 	   MAXIMUM_HISTOGRAM_VALUE_LENGTH);
   max_string[MAXIMUM_HISTOGRAM_VALUE_LENGTH] = NULL;
   _histogram_max = (int) atof(max_string);
 
   /* Now, set up the histogram array... */
 
   /* the histogram size is bytes 276 - 283, 8 bytes */
   memmove(histogram_size_string, 
 	   &data_histogram_record_data[HISTOGRAM_SIZE_OFFSET], 
 	   HISTOGRAM_SIZE_LENGTH);
   histogram_size_string[HISTOGRAM_SIZE_LENGTH] = NULL;
   _processed_histogram_size = atoi(histogram_size_string);
 
   /* Figure out if we can handle the histogram.... */
   if (_processed_histogram_size > 256) {
      make_announcement("Histogram is sized over 256, ignoring histogram...", NO_LOG);
   }
   else {
      /* Now assign values from the file to the histogram array */
      nth_histogram_value_string[FIRST_HISTOGRAM_VALUE_LENGTH] = NULL;
      
      for (i = 0; i < _processed_histogram_size ; i++) {
 	 memmove(nth_histogram_value_string, 
 	    &data_histogram_record_data[FIRST_HISTOGRAM_VALUE_OFFSET + (i*8)],
 	    8);
 	 _processed_histogram_array[i] = atoi(nth_histogram_value_string);	
      }
       
      /* Find the locations of the histogram max & min ... */
      _histogram_max = _histogram_min = 0;
      for (i = 0; i < (_processed_histogram_size - 1) ; i++) {
         if (_processed_histogram_array[i] >= _histogram_max) {
            _histogram_max = _processed_histogram_array[i];
            _histogram_max_location = i;
         }
      }
       
      for (i = (_processed_histogram_size - 1); i >= 0 ; i--) {
         if (_processed_histogram_array[i] <= _histogram_min) {
            _histogram_min = _processed_histogram_array[i];
            _histogram_min_location = i;
         }
      }
      /* recalculate _data_mean for SSP and PP from histogram */
      if (_is_sys_pp == 1) { /* PP */
         double dsum=0, sum=0;
         if (_is_sys_ramp == 2) { /* PP COMPLEX */
            for (i=0; i<128; i++) {
               dsum += (double) _processed_histogram_array[i]
                     * ((double) (128-i)*256-128);
               sum  += (double) _processed_histogram_array[i];
            }
            for (i=0; i<128; i++) {
               dsum += (double) _processed_histogram_array[128+i]
                     * ((double) (i*256+128));
               sum  += (double) _processed_histogram_array[128+i];
            }
            _data_mean = _data_mean_pp = dsum / sum;
            printf("\n_data_mean = %f, dsum = %f, sum = %f, ramp = %d", _data_mean, dsum, sum, _is_sys_ramp);
         }
         else if (_is_sys_ramp == 1){ /* PP RAMP */
            for (i=0; i<256; i++) {
               dsum += (double) _processed_histogram_array[i]
                     * ((double) i*256+128);
               sum  += (double) _processed_histogram_array[i];
            }
            _data_mean = _data_mean_pp = dsum / sum;
         }
            printf("\n_data_mean = %f, dsum = %f, sum = %f, ramp = %d", _data_mean, dsum, sum, _is_sys_ramp);
      }
   }

   free(data_histogram_record_data);
}

void
interpret_signal_data_histogram_record(leader_file_descriptor)
   int leader_file_descriptor;
{

   int   histogram_offset_amount,
         number_of_bytes_read;
 
   int   data_histogram_record_size,
         i_offset_q, i;
 
   char  min_string[MINIMUM_HISTOGRAM_VALUE_LENGTH + 1],
         max_string[MAXIMUM_HISTOGRAM_VALUE_LENGTH + 1],
         mean_string[MEAN_SAMPLE_VALUE_LENGTH + 1],
         sdev_string[SDEV_SAMPLE_VALUE_LENGTH + 1], 
         low_stretch_string[MINIMUM_SAMPLE_VALUE_LENGTH + 1],
         high_stretch_string[MAXIMUM_SAMPLE_VALUE_LENGTH + 1];

   char  histogram_size_string[HISTOGRAM_SIZE_LENGTH + 1],
         nth_histogram_value_string[FIRST_HISTOGRAM_VALUE_LENGTH + 1],  
         samples_per_group_string[SAMPLES_PER_GROUP_LENGTH + 1];
 
   byte  *data_histogram_record_data,
         leader_file_descriptor_record_data[LEADER_FILE_DESCRIPTOR_RECORD_SIZE];
 

   /* read in the first record (720 bytes), starting from the beginning */
   lseek(leader_file_descriptor, 0, 0);
 
   number_of_bytes_read = read(leader_file_descriptor,
 			       leader_file_descriptor_record_data,
 			       LEADER_FILE_DESCRIPTOR_RECORD_SIZE);
 
   if (number_of_bytes_read != LEADER_FILE_DESCRIPTOR_RECORD_SIZE) {
       char read_announcement[200];
       sprintf(read_announcement, 
 	       "Can't read first record of leader file (%d bytes)", 
 	       LEADER_FILE_DESCRIPTOR_RECORD_SIZE);
       make_announcement(read_announcement, LOG);
       exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }
   
   /* compute the offset from the leader file to the data histogram */
   histogram_offset_amount = 
      compute_offset_amount(leader_file_descriptor_record_data,
                            COMPUTE_HISTOGRAM_OFFSET);
 
   /* check to see how many histograms are given... */
   _number_of_histogram_records = 
     read_n_bytes(NUMBER_OF_DATA_HISTOGRAM_RECORDS_LENGTH, 
		  NUMBER_OF_DATA_HISTOGRAM_RECORDS_OFFSET, 
 		   leader_file_descriptor_record_data,
		  "number of data histogram records",
		  CAN_BE_ZERO);
 
   if (_number_of_histogram_records < 2) {
     /* If there are fewer than 2 records, then there isn't a signal
	data histogram record. */
      return;
   }
 
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
      make_announcement("Can't malloc.", LOG);
      exit(QC_MALLOC_ERROR);
   }
 
   /* 
    * signal data histogram exists, process the signal data histogram 
    */
   lseek(leader_file_descriptor, histogram_offset_amount, 0);

   /* read in the signal data histogram */
   number_of_bytes_read = read(leader_file_descriptor,
                               data_histogram_record_data,
                               data_histogram_record_size);

   if (number_of_bytes_read != data_histogram_record_size) {
      char read_announcement[200];
      sprintf(read_announcement,
              "Can't read signal histogram record in leader file (%d bytes)",
              data_histogram_record_size);
      make_announcement(read_announcement, LOG);
      exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }

   /* Find the histogram description */
   memmove(_i_histogram_description, 
           &data_histogram_record_data[I_HIST_DESC_OFFSET],
           I_HIST_DESC_LENGTH);
   format_description(_i_histogram_description);
 
   memmove(low_stretch_string,
           &data_histogram_record_data[MINIMUM_SAMPLE_VALUE_OFFSET],
           MINIMUM_SAMPLE_VALUE_LENGTH);
   low_stretch_string[MINIMUM_SAMPLE_VALUE_LENGTH] = NULL;
   _i_low_pixel_stretch_value = (int) atof(low_stretch_string);
  
   /* the _i_high_stretch_value is bytes 148-163, 16 bytes */
   memmove(high_stretch_string,
           &data_histogram_record_data[MAXIMUM_SAMPLE_VALUE_OFFSET],
           MAXIMUM_SAMPLE_VALUE_LENGTH);
   high_stretch_string[MAXIMUM_SAMPLE_VALUE_LENGTH] = NULL;
   _i_high_pixel_stretch_value = (int) atof(high_stretch_string);

   /* Now, find the histogram max, min, and data mean, and sdev... */
   /* the mean is bytes 164-179, 16 bytes */
   memmove(mean_string,
           &data_histogram_record_data[MEAN_SAMPLE_VALUE_OFFSET],
           MEAN_SAMPLE_VALUE_LENGTH);
   mean_string[MEAN_SAMPLE_VALUE_LENGTH] = NULL;
   _i_data_mean = atof(mean_string);

   /* the standard deviation is bytes 180-195, 16 bytes */
   memmove(sdev_string,
           &data_histogram_record_data[SDEV_SAMPLE_VALUE_OFFSET],
           SDEV_SAMPLE_VALUE_LENGTH);
   sdev_string[SDEV_SAMPLE_VALUE_LENGTH] = NULL;
   _i_data_sdev = atof(sdev_string);

   /* the min is bytes 212-227, 16 bytes */
   memmove(min_string, 
 	   &data_histogram_record_data[MINIMUM_HISTOGRAM_VALUE_OFFSET], 
 	   MINIMUM_HISTOGRAM_VALUE_LENGTH);
   min_string[MINIMUM_HISTOGRAM_VALUE_LENGTH] = NULL;
   _i_histogram_min = (int) atof(min_string);

   /* the max is bytes 228-243, 16 bytes */
   memmove(max_string, 
 	   &data_histogram_record_data[MAXIMUM_HISTOGRAM_VALUE_OFFSET], 
 	   MAXIMUM_HISTOGRAM_VALUE_LENGTH);
   max_string[MAXIMUM_HISTOGRAM_VALUE_LENGTH] = NULL;
   _i_histogram_max = (int) atof(max_string);

   /* the histogram size is bytes 276 - 283, 8 bytes */
   memmove(histogram_size_string,
           &data_histogram_record_data[HISTOGRAM_SIZE_OFFSET],
           HISTOGRAM_SIZE_LENGTH);
   histogram_size_string[HISTOGRAM_SIZE_LENGTH] = NULL;
   _i_histogram_size = atoi(histogram_size_string);

   /* calculate the offset between I_Hist_desc and Q_Hist_desc */
   i_offset_q = (FIRST_HISTOGRAM_VALUE_OFFSET - HISTOGRAM_TABLE_SET_OFFSET)
                + 8 * _i_histogram_size;

   /* Find the histogram description */
   memmove(_q_histogram_description, 
           &data_histogram_record_data[HIST_DESC_OFFSET + i_offset_q],
           HIST_DESC_LENGTH);
   format_description(_q_histogram_description);
 
   /* the _q_low_stretch_value is bytes 132-147, 16 bytes */
   memmove(low_stretch_string,
           &data_histogram_record_data[MINIMUM_SAMPLE_VALUE_OFFSET+i_offset_q],
           MINIMUM_SAMPLE_VALUE_LENGTH);
   low_stretch_string[MINIMUM_SAMPLE_VALUE_LENGTH] = NULL;
   _q_low_pixel_stretch_value = (int) atof(low_stretch_string);
  
   /* the _q_high_stretch_value is bytes 148-163, 16 bytes */
   memmove(high_stretch_string,
           &data_histogram_record_data[MAXIMUM_SAMPLE_VALUE_OFFSET+i_offset_q],
           MAXIMUM_SAMPLE_VALUE_LENGTH);
   high_stretch_string[MAXIMUM_SAMPLE_VALUE_LENGTH] = NULL;
   _q_high_pixel_stretch_value = (int) atof(high_stretch_string);

   /* Now, find the histogram max, min, and data mean, and sdev... */
   /* the mean is bytes 164-179, 16 bytes */
   memmove(mean_string,
           &data_histogram_record_data[MEAN_SAMPLE_VALUE_OFFSET+i_offset_q],
           MEAN_SAMPLE_VALUE_LENGTH);
   mean_string[MEAN_SAMPLE_VALUE_LENGTH] = NULL;
   _q_data_mean = atof(mean_string);

   /* the standard deviation is bytes 180-195, 16 bytes */
   memmove(sdev_string,
           &data_histogram_record_data[SDEV_SAMPLE_VALUE_OFFSET+i_offset_q],
           SDEV_SAMPLE_VALUE_LENGTH);
   sdev_string[SDEV_SAMPLE_VALUE_LENGTH] = NULL;
   _q_data_sdev = atof(sdev_string);

   /* the min is bytes 212-227, 16 bytes */
   memmove(min_string, 
           &data_histogram_record_data[MINIMUM_HISTOGRAM_VALUE_OFFSET 
                                       + i_offset_q],
           MINIMUM_HISTOGRAM_VALUE_LENGTH);
   min_string[MINIMUM_HISTOGRAM_VALUE_LENGTH] = NULL;
   _q_histogram_min = (int) atof(min_string);

   /* the max is bytes 228-243, 16 bytes */
   memmove(max_string,
           &data_histogram_record_data[MAXIMUM_HISTOGRAM_VALUE_OFFSET 
                                       + i_offset_q],
           MAXIMUM_HISTOGRAM_VALUE_LENGTH);
   max_string[MAXIMUM_HISTOGRAM_VALUE_LENGTH] = NULL;
   _q_histogram_max = (int) atof(max_string);

   /* the histogram size is bytes 276 - 283, 8 bytes */
   memmove(histogram_size_string,
           &data_histogram_record_data[HISTOGRAM_SIZE_OFFSET 
                                       + i_offset_q],
           HISTOGRAM_SIZE_LENGTH);
   histogram_size_string[HISTOGRAM_SIZE_LENGTH] = NULL;
   _q_histogram_size = atoi(histogram_size_string);

   if ((_i_histogram_size > 256) ||
       (_q_histogram_size > 256)) {
      make_announcement("Signal histogram is sized over 256, ignoring...", NO_LOG);
   }
   else
     {
       /* Now assign values from the file to the histogram I array */
       nth_histogram_value_string[FIRST_HISTOGRAM_VALUE_LENGTH] = NULL;
       
       for (i = 0; i < _i_histogram_size ; i++) {
         memmove(nth_histogram_value_string,
		 &data_histogram_record_data[FIRST_HISTOGRAM_VALUE_OFFSET
					    + (i*8)],
		 8);
         _i_histogram_array[i] = atoi(nth_histogram_value_string);
       }
       
       /* Now assign values from the file to the histogram Q array */
       for (i = 0; i < _q_histogram_size ; i++) {
         memmove(nth_histogram_value_string,
		 &data_histogram_record_data[FIRST_HISTOGRAM_VALUE_OFFSET
					    + i_offset_q
					    + (i*8)],
		 8);
         _q_histogram_array[i] = atoi(nth_histogram_value_string);
       }
       
       /* Find the locations of the I histogram max & min ... */
       _i_histogram_max = _i_histogram_min = 0;
       for (i = 0; i < (_i_histogram_size - 1) ; i++) {
         if (_i_histogram_array[i] >= _i_histogram_max) {
	   _i_histogram_max = _i_histogram_array[i];
	   _i_histogram_max_location = i + _i_low_pixel_stretch_value;
         }
       }
       
       for (i = (_i_histogram_size - 1); i >= 0 ; i--) {
         if (_i_histogram_array[i] <= _i_histogram_min) {
	   _i_histogram_min = _i_histogram_array[i];
	   _i_histogram_min_location = i + _i_low_pixel_stretch_value;
         }
       }
       
       /* Find the locations of the Q histogram max & min ... */
       _q_histogram_max = _q_histogram_min = 0;
       for (i = 0; i < (_q_histogram_size - 1) ; i++) {
         if (_q_histogram_array[i] >= _q_histogram_max) {
	   _q_histogram_max = _q_histogram_array[i];
	   _q_histogram_max_location = i + _q_low_pixel_stretch_value;
         }
       }
       
       for (i = (_q_histogram_size - 1); i >= 0 ; i--) {
         if (_q_histogram_array[i] <= _q_histogram_min) {
	   _q_histogram_min = _q_histogram_array[i];
	   _q_histogram_min_location = i + _q_low_pixel_stretch_value;
         }
       }
     }
   
   free(data_histogram_record_data);
}

void
get_projection_from_fdr(byte *leader_file_descriptor_record_data,
			int   leader_file_descriptor)
{

  byte *fdr_data ;
  int  fdr_offset_amount,
       number_of_bytes_read,
       number_of_facility_data_records;

  char orig_projection_string[GND_SLANT_FLAG_LENGTH + 2];

  
  /* Now, compute how far you have to jump in the leader file to get
     to the fdr */
  fdr_offset_amount =
    compute_offset_amount(leader_file_descriptor_record_data,
			  COMPUTE_FACILITY_DATA_RECORD_OFFSET);
  
  /* Check to see how many facility data records are given.. */
  number_of_facility_data_records = 
    read_n_bytes(NUMBER_OF_FACILITY_DATA_RECORDS_LENGTH, 
		 NUMBER_OF_FACILITY_DATA_RECORDS_OFFSET, 
		 leader_file_descriptor_record_data,
		 "number of facility data records",
		 CAN_BE_ZERO);
  
  if (number_of_facility_data_records < 1) {
    _projection_string = (char *) malloc (MAP_DESC_LENGTH * sizeof (char));
    strcpy (_projection_string, "UNKNOWN");
    return ;
  }
    
 
   /* jump to the map_projection_record */
   lseek(leader_file_descriptor, fdr_offset_amount, 0);
 
   fdr_data = 
     (byte *) malloc(FACILITY_DATA_RECORD_SIZE * sizeof(byte));
 
   if (!fdr_data) {
      char error[333];
      sprintf(error, "Cannot allocate %d bytes for facility data record record",
              FACILITY_DATA_RECORD_SIZE);
      make_announcement(error, LOG);
      exit(QC_MALLOC_ERROR);
   }
 
   _projection_string = (char *) malloc((MAP_DESC_LENGTH + 1) * sizeof(char));
   if (!_projection_string) {
      make_announcement("Couldn't allocate a few bytes", LOG);
      free(fdr_data);
      exit(QC_MALLOC_ERROR);
   }

   /* 
    * read facility_data_record_size bytes from leader_file_descriptor, and
    * place into facility_data_record_data.  Return the number of bytes
    * that were read. 
    */
   number_of_bytes_read = read(leader_file_descriptor, 
			       fdr_data, 
			       FACILITY_DATA_RECORD_SIZE);
   
   if (number_of_bytes_read != FACILITY_DATA_RECORD_SIZE) {
      char read_announcement[200];
      sprintf(read_announcement,
	      "Can't read facility data record in leader file (%d bytes)", 
 	      FACILITY_DATA_RECORD_SIZE);
      make_announcement(read_announcement, LOG);
      free(fdr_data);
      /* This should no longer exit */
      /* exit(QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR); */
      strcpy (_projection_string, "UNKNOWN");
      return ;
   }

   /* the projection is bytes 413-444, 32 bytes */
    memmove(orig_projection_string, 
 	   &fdr_data[GND_SLANT_FLAG_OFFSET], 
 	   GND_SLANT_FLAG_LENGTH);

    null_terminate_properly(orig_projection_string,
			    _projection_string,
			    GND_SLANT_FLAG_LENGTH);

    /* And we're all done with this... */
    free(fdr_data);

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
 
 
   if (offset_to_compute == COMPUTE_MAP_PROJECTION_OFFSET) {
      return return_offset_amount;
   }
 
   /* offset #2:  Map projection data records */
   N_of_records = read_n_bytes(NUMBER_OF_MAP_PROJECTION_RECORDS_LENGTH, 
 			       NUMBER_OF_MAP_PROJECTION_RECORDS_OFFSET, 
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
 
   if (offset_to_compute == COMPUTE_HISTOGRAM_OFFSET) {
     return return_offset_amount;
   }

   /* offset #8: data histogram record */
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
   
   return_offset_amount += (N_of_records * record_size);

   /* offset #9: the range spectra record */
   N_of_records = read_n_bytes(NUMBER_OF_RANGE_SPECTRA_RECORDS_LENGTH, 
 			       NUMBER_OF_RANGE_SPECTRA_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of range spectra records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(RANGE_SPECTRA_RECORDS_SIZE_LENGTH, 
 		              RANGE_SPECTRA_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "range spectra record size",
 		              CAN_BE_ZERO);

   return_offset_amount += (N_of_records * record_size);


   /* Offset #10: the DEM descriptor record */
   N_of_records = read_n_bytes(NUMBER_OF_DEM_DESCRIPTOR_RECORDS_LENGTH, 
 			       NUMBER_OF_DEM_DESCRIPTOR_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of DEM descriptor records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(DEM_DESCRIPTOR_RECORDS_SIZE_LENGTH, 
 		              DEM_DESCRIPTOR_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "DEM descriptor record size",
 		              CAN_BE_ZERO);

   return_offset_amount += (N_of_records * record_size);

   /* offset #11: the radar parameter record */
   N_of_records = read_n_bytes(NUMBER_OF_RADAR_PARAMETER_RECORDS_LENGTH, 
 			       NUMBER_OF_RADAR_PARAMETER_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of radar parameter records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(RADAR_PARAMETER_RECORDS_SIZE_LENGTH, 
 		              RADAR_PARAMETER_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "radar parameter record size",
 		              CAN_BE_ZERO);

   return_offset_amount += (N_of_records * record_size);


   /* offset #12: the annotation data record */
   N_of_records = read_n_bytes(NUMBER_OF_ANNOTATION_DATA_RECORDS_LENGTH, 
 			       NUMBER_OF_ANNOTATION_DATA_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of annotation data records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(ANNOTATION_DATA_RECORDS_SIZE_LENGTH, 
 		              ANNOTATION_DATA_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "annotation data record size",
 		              CAN_BE_ZERO);

   return_offset_amount += (N_of_records * record_size);

   
   /* offset #13: the detailed processing parameter record */
   N_of_records = read_n_bytes(NUMBER_OF_DETAILED_PROCESSING_RECORDS_LENGTH, 
 			       NUMBER_OF_DETAILED_PROCESSING_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of detailed processing parameter records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(DETAILED_PROCESSING_RECORDS_SIZE_LENGTH, 
 		              DETAILED_PROCESSING_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "detailed processing parameter record size",
 		              CAN_BE_ZERO);

   return_offset_amount += (N_of_records * record_size);


   /* offset 14: the calibration record */
   N_of_records = read_n_bytes(NUMBER_OF_CALIBRATION_RECORDS_LENGTH, 
 			       NUMBER_OF_CALIBRATION_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of calibration records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(CALIBRATION_RECORDS_SIZE_LENGTH, 
 		              CALIBRATION_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "calibration record size",
 		              CAN_BE_ZERO);

   return_offset_amount += (N_of_records * record_size);

   /* Then, the GCP record */
   N_of_records = read_n_bytes(NUMBER_OF_GCP_RECORDS_LENGTH, 
 			       NUMBER_OF_GCP_RECORDS_OFFSET, 
 			       byte_data,
 			       "number of GCP records",
 			       CAN_BE_ZERO);
 
   record_size = read_n_bytes(GCP_RECORDS_SIZE_LENGTH, 
 		              GCP_RECORDS_SIZE_OFFSET,
 		              byte_data,
 		              "GCP record size",
 		              CAN_BE_ZERO);

   return_offset_amount += (N_of_records * record_size);

   /* That was the last of 'em */

   if (offset_to_compute == COMPUTE_FACILITY_DATA_RECORD_OFFSET) {
     return return_offset_amount;
   }
   

   /* If we've gotten here, then there's a problem. */
   make_announcement("Compute_offset called incorrectly!", LOG);
   exit(QC_PROGRAMMER_ERROR);
}



