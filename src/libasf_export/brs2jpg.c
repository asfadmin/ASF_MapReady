#include "asf.h"
#include "asf_meta.h"
#include "asf_export.h"

int brs2jpg(char *browseFile, char *workreportFile, char *outFile)
{
  FILE *fp;
  char *baseName, tmpDataFile[1024], tmpDir[1024], line[1024];
  meta_parameters *meta;

  // Create temporary directory
  baseName = get_basename(outFile);
  strcpy(tmpDir, baseName);
  strcat(tmpDir, "-");
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);

  // Generate ASF internal file
  meta = raw_init();
  meta->general->image_data_type = BROWSE_IMAGE;
  fp = FOPEN(workreportFile, "r");
  while (fgets(line, 1024, fp)) {
    if (strncmp(line, "Brs_NoOfBrowseLines", 19) == 0)
      sscanf(line, "Brs_NoOfBrowseLines=\"%d\"", &meta->general->line_count);
    else if (strncmp(line, "Brs_NoOfBrowsePixels", 20) == 0)
      sscanf(line, "Brs_NoOfBrowsePixels=\"%d\"", &meta->general->sample_count);
    else if (strncmp(line, "Brs_BrowseBitPixel=\"8\"", 22) == 0)
      meta->general->data_type = BYTE;
  }
  FCLOSE(fp);
  sprintf(tmpDataFile, "%s/%s.img", tmpDir, baseName);
  meta_write(meta, tmpDataFile);
  fileCopy(browseFile, tmpDataFile);

  // Export to JPEG and clean up
  asf_export(JPEG, SIGMA, tmpDataFile, outFile);
  remove_dir(tmpDir);
  meta_free(meta);
  
  return EXIT_SUCCESS;
}
