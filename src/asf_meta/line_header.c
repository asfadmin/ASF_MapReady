#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "line_header.h"

static int intValue(FILE *fp, int offset, int bytes)
{
  unsigned char value[4];

  FSEEK64(fp, offset+bytes-1, SEEK_SET);
  FREAD(&value, 1, 4, fp);
  return bigInt32(value);
}

static short int shortValue(FILE *fp, int offset, int bytes)
{
  unsigned char value[4];

  FSEEK64(fp, offset+bytes-1, SEEK_SET);
  FREAD(&value, 1, 4, fp);
  return bigInt16(value);
}

alos_processed_line_t *read_alos_proc_line_header(const char *inName, 
						  int line_number)
{
  FILE *fp;
  alos_processed_line_t *line = 
    (alos_processed_line_t *) MALLOC(sizeof(alos_processed_line_t));
  struct HEADER hdr;
  int offset, length, current_line;

  // Determine offset and jump to it
  offset = firstRecordLen((char *)inName);
  fp = FOPEN(inName, "rb");
  current_line = 0;
  while (current_line < line_number) {
    
    // Read the header information
    FSEEK64(fp, offset, SEEK_SET);
    FREAD(&hdr, 1, 12, fp);
    length = bigInt32(hdr.recsiz);
    offset += length;
    current_line++;
  }

  line->line_num = intValue(fp, offset, 13);
  line->rec_num = intValue(fp, offset, 17);
  line->n_left_pixel = intValue(fp, offset, 21);
  line->n_data_pixel = intValue(fp, offset, 25);
  line->n_right_pixel = intValue(fp, offset, 29);
  line->sensor_updf = intValue(fp, offset, 33);
  line->acq_year = intValue(fp, offset, 37);
  line->acq_day = intValue(fp, offset, 41);
  line->acq_msec = intValue(fp, offset, 45);
  line->sar_chan_id = shortValue(fp, offset, 49);
  line->sar_chan_code = shortValue(fp, offset, 51);
  line->tran_polar = shortValue(fp, offset, 53);
  line->recv_polar = shortValue(fp, offset, 55);
  line->prf = intValue(fp, offset, 57);
  line->scan_id = intValue(fp, offset, 61);
  line->sr_first = intValue(fp, offset, 65);
  line->sr_mid = intValue(fp, offset, 69);
  line->sr_last = intValue(fp, offset, 73);
  line->fdc_first = intValue(fp, offset, 77);
  line->fdc_mid = intValue(fp, offset, 81);
  line->fdc_last = intValue(fp, offset, 85);
  line->ka_first = intValue(fp, offset, 89);
  line->ka_mid = intValue(fp, offset, 93);
  line->ka_last = intValue(fp, offset, 97);
  line->nadir_ang = intValue(fp, offset, 101)/1000000;
  line->squint_ang = intValue(fp, offset, 105)/1000000;;
  line->geo_updf = intValue(fp, offset, 129);
  line->lat_first = intValue(fp, offset, 133)/1000000;
  line->lat_mid = intValue(fp, offset, 137)/1000000;
  line->lat_last = intValue(fp, offset, 141)/1000000;
  line->long_first = intValue(fp, offset, 145)/1000000;
  line->long_mid = intValue(fp, offset, 149)/1000000;
  line->long_last = intValue(fp, offset, 153)/1000000;
  line->north_first = intValue(fp, offset, 157);
  line->north_last = intValue(fp, offset, 165);
  line->east_first = intValue(fp, offset, 169);
  line->east_last = intValue(fp, offset, 177);
  line->heading = intValue(fp, offset, 181)/1000000;
  
  FCLOSE(fp);
  
  return line;
}

rsat_processed_line_t *read_rsat_proc_line_header(const char *inName, 
						  int line_number)
{
  FILE *fp;
  rsat_processed_line_t *line = 
    (rsat_processed_line_t *) MALLOC(sizeof(rsat_processed_line_t));
  struct HEADER hdr;
  int offset, length, current_line;

  // Determine offset and jump to it
  offset = firstRecordLen((char *)inName);
  fp = FOPEN(inName, "rb");
  current_line = 0;
  while (current_line < line_number) {
    
    // Read the header information
    FSEEK64(fp, offset, SEEK_SET);
    FREAD(&hdr, 1, 12, fp);
    length = bigInt32(hdr.recsiz);
    offset += length;
    current_line++;
  }

  line->line_num = intValue(fp, offset, 13);
  line->rec_num = intValue(fp, offset, 17);
  line->n_left_pixel = intValue(fp, offset, 21);
  line->n_data_pixel = intValue(fp, offset, 25);
  line->n_right_pixel = intValue(fp, offset, 29);
  line->sensor_updf = intValue(fp, offset, 33);
  line->acq_year = intValue(fp, offset, 37);
  line->acq_day = intValue(fp, offset, 41);
  line->acq_msec = intValue(fp, offset, 45);
  line->sar_chan_id = shortValue(fp, offset, 49);
  line->sar_chan_code = shortValue(fp, offset, 51);
  line->tran_polar = shortValue(fp, offset, 53);
  line->recv_polar = shortValue(fp, offset, 55);
  line->prf = intValue(fp, offset, 57);
  line->sr_first = intValue(fp, offset, 65);
  line->sr_mid = intValue(fp, offset, 69);
  line->sr_last = intValue(fp, offset, 73);
  line->fdc_first = intValue(fp, offset, 77);
  line->fdc_mid = intValue(fp, offset, 81);
  line->fdc_last = intValue(fp, offset, 85);
  line->ka_first = intValue(fp, offset, 89);
  line->ka_mid = intValue(fp, offset, 93);
  line->ka_last = intValue(fp, offset, 97);
  line->nadir_ang = intValue(fp, offset, 101)/1000000;
  line->squint_ang = intValue(fp, offset, 105)/1000000;;
  line->null_f = intValue(fp, offset, 61);
  line->geo_updf = intValue(fp, offset, 129);
  line->lat_first = intValue(fp, offset, 133)/1000000;
  line->lat_mid = intValue(fp, offset, 137)/1000000;
  line->lat_last = intValue(fp, offset, 141)/1000000;
  line->long_first = intValue(fp, offset, 145)/1000000;
  line->long_mid = intValue(fp, offset, 149)/1000000;
  line->long_last = intValue(fp, offset, 153)/1000000;
  line->north_first = intValue(fp, offset, 157);
  line->north_last = intValue(fp, offset, 165);
  line->east_first = intValue(fp, offset, 169);
  line->east_last = intValue(fp, offset, 177);
  line->heading = intValue(fp, offset, 181)/1000000;
  
  FCLOSE(fp);
  
  return line;
}
