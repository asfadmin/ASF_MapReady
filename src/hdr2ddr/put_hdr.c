
#include "asf.h"
#include "hdr.h"


/* Write .hdr file based on the struct */
void put_hdr(char *hdr_name, jpl_header *hdr)
{
  FILE *fp = FOPEN(hdr_name,"wb");
  fprintf(fp,"%d                     ; Magnitude Bytes per Pixel\n",
          hdr->magnitude_bytes);
  fprintf(fp,"%d                     ; Elevation Bytes per Pixel\n",
          hdr->elevation_bytes);
  fprintf(fp,"%s                   ; Data file type\n",
          hdr->data_type);
  fprintf(fp,"       %lf          %lf        ; Elevation Scale and Shift (M)\n",
          hdr->elevation_scale, hdr->elevation_shift);
  fprintf(fp,"      %.12lf    %.12lf  ; Post Spacing (Deg)\n",
          hdr->line_increment, hdr->sample_increment);
  fprintf(fp,"      %.8lf      %.8lf      ; Starting corner position (s,c)\n",
          hdr->start_lat, hdr->start_lon);
  fprintf(fp,"     %d             %d                ; Data file dimensions\n",
          hdr->line_count, hdr->sample_count);
}
