#include "asf_sar.h"
#include "asf_raster.h"
#include <assert.h>

FILE *fopen_cla_file(const char *file, const char *mode)
{
  FILE *fp;

  // try opening as-is (file is in the current directory)
  //printf("trying: %s\n", file);
  fp = fopen(file,mode);
  if (fp)
    return fp;

  // failed-- try ".cla" extension
  char *with_ext = appendExt(file, ".cla");
  //printf("trying: %s\n", with_ext);
  fp = fopen(with_ext,mode);
  if (fp) {
    free(with_ext);
    return fp;
  }

  // failed-- try opening in the share dir
  int l = strlen(get_asf_share_dir());
  char *share_file = MALLOC(sizeof(char)*(l+strlen(file)+32));
  sprintf(share_file, "%s/classifications/%s", get_asf_share_dir(), file);
  fp = fopen(share_file,mode);
  //printf("trying: %s\n", share_file);
  if (fp) {
    free(with_ext);
    free(share_file);
    return fp;
  }

  // failed-- try opening in the share dir with ".cla" added
  sprintf(share_file, "%s/classifications/%s", get_asf_share_dir(), with_ext);
  //printf("trying: %s\n", share_file);
  fp = fopen(share_file,mode);
  free(with_ext);
  free(share_file);

  if (fp)
    return fp;

  // failed-- abort with an error
  asfPrintError("Could not find classification file: %s\n", file);

  // not reached
  return NULL; 
}

classifier_t *read_classifier(const char *classFile)
{
  FILE *fp = fopen_cla_file(classFile, "r");

  int nlines = 0;
  char line[1024];

  while (NULL!=fgets(line,1023,fp)) {
    if (line[0]=='#') continue;
    ++nlines;
  }
  fclose(fp);

  classifier_t *classifier = MALLOC(sizeof(classifier_t));
  classifier->entropy_min = MALLOC(sizeof(double)*nlines);
  classifier->entropy_max = MALLOC(sizeof(double)*nlines);
  classifier->anisotropy_min = MALLOC(sizeof(double)*nlines);
  classifier->anisotropy_max = MALLOC(sizeof(double)*nlines);
  classifier->alpha_min = MALLOC(sizeof(double)*nlines);
  classifier->alpha_max = MALLOC(sizeof(double)*nlines);
  classifier->greyscale_value = MALLOC(sizeof(int)*nlines);

  int num_read = 0, line_num = 0;
  fp = fopen_cla_file(classFile, "r");
  while (NULL!=fgets(line,1023,fp)) {
    ++line_num;

    if (line[0]=='#')
      continue;

    double entropy_min, entropy_max;
    double anisotropy_min, anisotropy_max;
    double alpha_min, alpha_max;
    int greyscale_value;

    int n =
      sscanf(line,"%lf %lf %lf %lf %lf %lf %d\n",
             &entropy_min,&entropy_max,
             &alpha_min,&alpha_max,
             &anisotropy_min,&anisotropy_max,
             &greyscale_value);

    if (n != 7) {
      asfPrintWarning("Classification file '%s': line %d, too few values.\n",
                      classFile, line_num);
    }
    else {
      assert (num_read < nlines);
      classifier->entropy_min[num_read] = entropy_min;
      classifier->entropy_max[num_read] = entropy_max;
      classifier->anisotropy_min[num_read] = anisotropy_min;
      classifier->anisotropy_max[num_read] = anisotropy_max;
      classifier->alpha_min[num_read] = alpha_min;
      classifier->alpha_max[num_read] = alpha_max;
      classifier->greyscale_value[num_read] = greyscale_value;
      ++num_read;
    }
  }

  classifier->n_classes = num_read;

  fclose(fp);
  return classifier;
}

void free_classifier(classifier_t *classifier)
{
  if (classifier) {
    free(classifier->entropy_min);
    free(classifier->entropy_max);
    free(classifier->anisotropy_min);
    free(classifier->anisotropy_max);
    free(classifier->alpha_min);
    free(classifier->alpha_max);
    free(classifier);
  }
}

int classify(classifier_t *classifier, float entropy, float anisotropy,
             float alpha)
{
  int i;

  for (i=0; i<classifier->n_classes; ++i) {

    if (entropy    >= classifier->entropy_min[i] &&
        entropy    <= classifier->entropy_max[i] &&
        anisotropy >= classifier->anisotropy_min[i] &&
        anisotropy <= classifier->anisotropy_max[i] &&
        alpha      >= classifier->alpha_min[i] &&
        alpha      <= classifier->alpha_max[i])
    {
      return classifier->greyscale_value[i];
    }
  }

  printf("Value not classified: E:%f An:%f Al:%f\n",
         entropy, anisotropy, alpha);

  for (i=0; i<classifier->n_classes; ++i) {

    printf("%d: E: %f %f An:%f %f Al:%f %f --> %d\n", i,
           classifier->entropy_min[i],
           classifier->entropy_max[i],
           classifier->anisotropy_min[i],
           classifier->anisotropy_max[i],
           classifier->alpha_min[i],
           classifier->alpha_max[i],
           classifier->greyscale_value[i]);
  }

  return 0;
}
