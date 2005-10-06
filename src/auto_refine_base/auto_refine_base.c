#include "asf.h"
#include "ifm.h"

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

void usage(char* name)
{
  printf("\n"
	 "USAGE:\n"
	 "%s [-tail_clip <val>] [-end_dif <val>]\n"
	 "   [-min_iter <val>] [-max_iter <val>]\n"
	 "   [-max_rem <val>]\n"
	 "   phase_file seeds_file meta_file old_base_file new_base_file\n"
	 "\n"
	 "phase_file:    Unwrapped interferogram phase file\n"
	 "seeds_file:    Tie-point location file\n"
	 "meta_file:     Metadata file for interferogram\n"
	 "old_base_file: Baseline file containing baseline to refine\n"
	 "new_base_file: Name of new baseline file to be created\n"
	 "-tail_clip:    Clip <val> percent of points each iteration (def. 2.5)\n"
	 "-end_dif:      When difference between successive baselines is less than <val>,\n"
	 "                  stop iterating (def. 1.0)\n"
	 "-min_iter:     Iterate at least <val> times. This option supercedes -end_dif\n"
	 "                  (def. 1)\n"
	 "-max_iter:     Iterate at most <val> times. This option supercedes -end_dif\n"
	 "                  (def. 100)\n"
	 "-max_rem:      Remove at most <val> percent of points from original seeds list\n"
	 "                  (def. 50.0)\n"
	 "-keep:         Keep retains all intermediate baseline and seed files (def. off)\n"
	 "-debug:        Debug retains ALL intermediate files, generates very verbose\n"
	 "                  output, and implies the -keep option (def. off)\n"
	 "Version 1.0\n\n", name);
  exit(EXIT_FAILURE);
}


int my_comp(double *v1, double *v2)
{
  if(*v1 < *v2)
    return(1);
  if(*v1 == *v2)
    return(0);
  if(*v1 > *v2)
    return(-1);
  return(0);
}


/*
  This is essentially a duplicate of test_base.c from refine_base.
  The only difference here is a lack of output.
*/
int* auto_test_base(char *basefile, char *matfile, char *vecfile, int iter_count, double tail_clip, int *num_clip_points, double **difs, int debug)
{
  int i, j;
  int m, n = 4;
  double sum, errsum;
  double **A;
  double *b, *x, *Ax;
  FILE *fp;
  double *sorted_difs;
  double threshold;
  int clip_points;
  int *clip_list;

  if(debug == TRUE)
  {
    printf("auto_test_base() entered. Checking for presence of files...\n");
    fflush(NULL);
  }

  if(!fileExist(matfile))
    Exit("   auto_test_base:  <A> dne");
  if(!fileExist(basefile))
    Exit("   auto_test_base:  <x> dne");
  if(!fileExist(vecfile))
    Exit("   auto_test_base:  <b> dne");
  m = fileNumLines(matfile);
  if(fileNumLines(vecfile) != m)
    Exit("   auto_test_base: Number of rows in matrix A and vector B do not match");

  if(debug == TRUE)
  {
    printf("   Necessary files found.\nAllocating space for matrices and vectors\n");
    fflush(NULL);
  }

  if(debug == TRUE)
  {
    printf("Calculating number of points to clip and making space\n");
    fflush(NULL);
  }
  clip_points = 2 * ceil(fabs(((double)m * tail_clip) / 100.0));
  *num_clip_points = clip_points;
  *difs = (double*)malloc(sizeof(double) * m);
  sorted_difs = (double*)malloc(sizeof(double) * m);

  if(difs == NULL || sorted_difs == NULL)
  {
    printf("\nFATAL ERROR: unable to allocate %d bytes!\n\n", sizeof(double) * clip_points);
    exit(0);
  }

  if(debug == TRUE)
  {
    printf("We will be clipping %d points this iteration\n", clip_points);
    fflush(NULL);
  }
  A = dmatrix(1, m, 1, n);
  b = dvector(1, m);
  x = dvector(1, n);
  Ax = dvector(1, m);

  if(debug == TRUE)
  {
    printf("Filling matrices and vectors with file data...\n");
    fflush(NULL);
  }
  fp = FOPEN(matfile, "r");
  for(i = 1; i <= m; ++i)
  {
    for(j = 1; j <= n; ++j)
    {
      fscanf(fp, "%lf", &A[i][j]);
    }
  }
  fclose(fp);

  fp = FOPEN(basefile, "r");
  for(i = 1; i <= n; ++i)
  {
    fscanf(fp, "%lf", &x[i]);
  }
  fclose(fp);

  fp = FOPEN(vecfile, "r");
  for(i = 1; i <= m; ++i)
  {
    fscanf(fp, "%lf", &b[i]);
  }
  fclose(fp);

  if(debug == TRUE)
  {
    printf("Finding worst points...\n");
    fflush(NULL);
  }
  sum = 0.0;
  errsum = 0.0;
  for(j = 1; j <= m; ++j)
    Ax[j] = 0.0;
  for(i = 1; i <= m; i++)
  {
    for(j = 1; j <= n; j++)
    {
      Ax[i] += A[i][j] * x[j];
    }

    (*difs)[i - 1] = fabs(b[i] - Ax[i]);
    sorted_difs[i - 1] = (*difs)[i - 1];

    if(debug == TRUE)
    {
      printf("%03d  b: %lf   A*x: %lf   diff: %lf\n", i, b[i], Ax[i], (*difs)[i - 1]);
      fflush(NULL);
    }
    sum += (b[i] - Ax[i]) * (b[i] - Ax[i]);
    errsum += (b[i] - Ax[i]);
  }

  if(debug == TRUE)
  {
    printf("Freeing matrix and vector memory\n");
    fflush(NULL);
  }

  free_dmatrix(A, 1, m, 1, n);
  free_dvector(b, 1, m);
  free_dvector(x, 1, n);
  free_dvector(Ax, 1, m);

  if(debug == TRUE)
  {
    printf("Sorting difs so we can find the %d worst points\n", clip_points);
    fflush(NULL);
  }
  qsort(&sorted_difs[0], m, sizeof(double), (int (*)(const void *, const void *))my_comp);
  threshold = sorted_difs[clip_points - 1];

  printf("   Clipping %d points with threshold level %lf\n", *num_clip_points, threshold);

  free(sorted_difs);

  if(debug == TRUE)
  {
    printf("Building list of points to clip\n");
    fflush(NULL);
  }
  /* FIXME: clip_list used to get malloc'ed 'clip_points' integer sized bytes
   * FIXME: now it gets 'm' integer sized bytes. I didn't really spend much
   * FIXME: time figuring out whether or not this is the proper number of
   * FIXME: points to give it, but it seems to work without seg faulting */
  clip_list = (int*)malloc(sizeof(int) * m);
  if(clip_list == NULL)
  {
    printf("\nFATAL ERROR: Unable to allocate %d bytes!\n\n", sizeof(int) * clip_points);
    exit(0);
  }
  {
    int jj = 0;
    for(i = 0; i < m; ++i)
    {
      if((*difs)[i] >= threshold)
      {
	if (debug == TRUE) {
		printf("   Point #%d will be clipped with dif %lf\n", i + 1, (*difs)[i]);
		fflush(NULL);
	}
	clip_list[jj] = i + 1;
	++jj;
      }
    }
  }

  if(debug == TRUE)
  {
    printf("auto_test_base() completed, returning list of worst points\n");
    fflush(NULL);
  }
  return(clip_list);
}


int main(int argc, char *argv[])
{
  char phase_file[256], tp_file[256];
  char meta_file[256], oldbase_file[256], newbase_file[256];
  int iter_count;
  double enddif = 1.0;
  double tail_clip = 2.5;
  int keep = FALSE;
  int debug = FALSE;
  int min_iter = 1;
  int max_iter = 100;
  double max_rem = 50.0;
  int orig_num_points;
  double *difs;
  int do_iter = TRUE;

  /* parse command line args */
  currArg = 1;
  while(currArg < (argc - 5))
  {
    char *key = argv[currArg];

    if(strcmp(key, "-quiet") == 0)
    {
      quietflag = TRUE;
    }
    else if(strcmp(key, "-tail_clip") == 0)
    {
      tail_clip = atof(argv[++currArg]);
    }
    else if(strcmp(key, "-end_dif") == 0)
    {
      enddif = atof(argv[++currArg]);
    }
    else if(strcmp(key, "-keep") == 0)
    {
      keep = TRUE;
    }
    else if(strcmp(key, "-debug") == 0)
    {
      keep = TRUE;
      debug = TRUE;
    }
    else if(strcmp(key, "-max_rem") == 0)
    {
      max_rem = atof(argv[++currArg]);
    }
    else if(strcmp(key, "-min_iter") == 0)
    {
      min_iter = atoi(argv[++currArg]);
    }
    else if(strcmp(key, "-max_iter") == 0)
    {
      max_iter = atoi(argv[++currArg]);
    }
    else
    {
      printf("\nInvalid option: %s\n", argv[currArg - 1]);
      usage(argv[0]);
    }
    ++currArg;
  }

  if(max_iter < min_iter)
  {
    printf("-max_iter was specified with a value smaller than -min_iter! (%d < %d)...this simply will not work!\n", max_iter, min_iter);
    exit(0);
  }
  
  if((argc - currArg) < 5)
  {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }

  /* Required command line args */
  strcpy(phase_file, argv[currArg]);
  strcpy(tp_file, argv[currArg + 1]);
  strcpy(meta_file, argv[currArg + 2]);
  strcpy(oldbase_file, argv[currArg + 3]);
  strcpy(newbase_file, argv[currArg + 4]);

  orig_num_points = fileNumLines(tp_file);

  system("date");
  printf("Program: auto_refine_base\n\n");

  /* Prep for iteration */
  {
    char temp[256];

    if(debug == TRUE)
    {
      printf("Copying files %s and %s to %s.000 and %s.000\n", oldbase_file, tp_file, oldbase_file, tp_file);
      fflush(NULL);
    }
    
    sprintf(temp, "cp %s %s.000", oldbase_file, oldbase_file);
    system(temp);
    sprintf(temp, "cp %s %s.000", tp_file, tp_file);
    system(temp);
    iter_count = 0;
  }


  /* Main loop -- iterate over the data as necessary */
  while(iter_count < max_iter && do_iter == TRUE)
  {
    char temp_command[255];

    /* Run refine_base to generate a new baseline */
    sprintf(temp_command, "refine_base -keep %3d %s %s.%03d %s %s.%03d %s.%03d", 
	    iter_count, phase_file, tp_file, iter_count, meta_file,
	    oldbase_file, iter_count, oldbase_file, iter_count + 1);
    if(debug == TRUE)
    {
      printf("Running refine_base in main iteration loop, iteration %d\n"
	     "   Command line: %s\n", iter_count, temp_command);
      fflush(NULL);
    }
    system(temp_command);
    fflush(NULL);

    /* Check the baseline difference against enddif */
    {
      char temp_file_name[255];
      /* FIXME: As of the time of writing this, FCLOSE seems to be
	 exhibiting some strange behavior. Namely, a file is opened,
	 read from, and closed. Later, another file is open, read from,
	 (into a different buffer than the first file) and closed.
	 For some reason, upon that second call to FCLOSE, the first
	 value read from the first file gets hosed, as it was passed by
	 reference and FCLOSE apparently kept something around it ought
	 not have. So, I'm putting a buffer between FCLOSE and my data.
	 Bad FCLOSE! No donut!
      */
      double safety_buf[4];
      double a[4], b[4];
      FILE *temp_file;
      int ret_stat;
      double total_dif;

      if(debug == TRUE)
      {
        printf("Comparing new baseline values to old baseline values\n");
        fflush(NULL);
      }

      /* Read four values from the previous baseline file */
      sprintf(temp_file_name, "%s.%03d", oldbase_file, iter_count);
      if(debug == TRUE)
      {
        printf("   Reading old baseline values from file %s\n", temp_file_name);
        fflush(NULL);
      }
      temp_file = FOPEN(temp_file_name, "r");
      ret_stat = fscanf(temp_file, "%lf %lf %lf %lf", &safety_buf[0], &safety_buf[1], &safety_buf[2], &safety_buf[3]);
      a[0] = safety_buf[0];
      a[1] = safety_buf[1];
      a[2] = safety_buf[2];
      a[3] = safety_buf[3];
      FCLOSE(temp_file);
      if(ret_stat < 4)
      {
	printf("ERROR:\n"
	       "Unable to read four values from baseline file!\n"
	       "%d values read from %s\n",
	       ret_stat, temp_file_name);
	exit(EXIT_FAILURE);
      }
      if(debug == TRUE)
      {
        printf("      Old values: %lf %lf %lf %lf\n", a[0], a[1], a[2], a[3]);
        fflush(NULL);
      }

      /* Read four values from the newest baseline file */
      sprintf(temp_file_name, "%s.%03d", oldbase_file, iter_count + 1);
      if(debug == TRUE)
      {
        printf("   Reading new baseline values from file %s\n", temp_file_name);
        fflush(NULL);
      }
      temp_file = FOPEN(temp_file_name, "r");
      ret_stat = fscanf(temp_file, "%lf %lf %lf %lf", &safety_buf[0], &safety_buf[1], &safety_buf[2], &safety_buf[3]);
      b[0] = safety_buf[0];
      b[1] = safety_buf[1];
      b[2] = safety_buf[2];
      b[3] = safety_buf[3];
      FCLOSE(temp_file);
      if(ret_stat < 4)
      {
	printf("ERROR:\n"
	       "Unable to read four values from baseline file!\n"
	       "%d values read from %s\n",
	       ret_stat, temp_file_name);
	exit(EXIT_FAILURE);
      }
      if(debug == TRUE)
      {
        printf("      New values: %lf %lf %lf %lf\n", b[0], b[1], b[2], b[3]);
        fflush(NULL);
      }

      /* Find the difference between each component and total it up */
      if(debug == TRUE)
      {
        printf("   Finding total baseline difference\n");
	printf("      %3.6lf - %3.6lf = %3.6lf\n", a[0], b[0], a[0] - b[0]);
	printf("      %3.6lf - %3.6lf = %3.6lf\n", a[1], b[1], a[1] - b[1]);
	printf("      %3.6lf - %3.6lf = %3.6lf\n", a[2], b[2], a[2] - b[2]);
	printf("      %3.6lf - %3.6lf = %3.6lf\n", a[3], b[3], a[3] - b[3]);
        fflush(NULL);
      }
      total_dif =
	fabs(a[0] - b[0]) +
	fabs(a[1] - b[1]) +
	fabs(a[2] - b[2]) +
	fabs(a[3] - b[3]);

      printf("   Baseline difference: %lf\n", total_dif);
      printf("   Target difference:   %lf\n", enddif);

      /* If the total difference is < enddif, stop iterating */
      if(total_dif < enddif && iter_count > min_iter)
      {
	if(debug == TRUE)
	{
	  printf("      New baseline values within tolerances and minimum number of iterations performed, exiting main loop\n");
	  fflush(NULL);
	}
	if(keep == FALSE)
	{
	  char rem_command[256];
	  sprintf(rem_command, "rm -f %s.%03d", oldbase_file, iter_count);
	  printf("%s\n", rem_command);
	  system(rem_command);
	  if(iter_count > 0)
	  {
	    sprintf(rem_command, "rm -f %s.%03d", tp_file, iter_count - 1);
	    printf("%s\n", rem_command);
	    system(rem_command);
	  }
	}
	if(debug == FALSE)
	{
	  if(iter_count > 0)
	  {
	    char rem_command[256];
	    sprintf(rem_command, "rm -f matrix.%d", iter_count - 1);
	    printf("%s\n", rem_command);
	    system(rem_command);
            sprintf(rem_command, "rm -f ctrlpts.%d", iter_count - 1);
            printf("%s\n", rem_command);
            system(rem_command);
            sprintf(rem_command, "rm -f vecB.%d", iter_count - 1);
            printf("%s\n", rem_command);
            system(rem_command);
	  }
	}
	iter_count++;
	break;
      }
      if(debug == TRUE)
      {
        printf("      New baseline values not good enough, continuing\n");
        fflush(NULL);
      }
    }

    /* Find and remove the worst points */
    {
      FILE *oldtpt_file;
      char oldtpt_name[255];
      FILE *newtpt_file;
      char newtpt_name[255];
      char test_base_file[255], test_matrix_file[255], test_vec_file[255];
      int *clip_list;
      int num_clip_points;

      sprintf(test_base_file, "%s.%03d", oldbase_file, iter_count + 1);
      sprintf(test_matrix_file, "matrix.%d", iter_count);
      sprintf(test_vec_file, "vecB.%d", iter_count);
      if(debug == TRUE)
      {
        printf("Finding worst points based on:\n");
        printf("   Basefile:      %s\n", test_base_file);
        printf("   Matrix file:   %s\n", test_matrix_file);
        printf("   VecB file:     %s\n", test_vec_file);
        fflush(NULL);
      }
      clip_list = auto_test_base(test_base_file, test_matrix_file, test_vec_file, iter_count, tail_clip, &num_clip_points, &difs, debug);

      if(debug == TRUE)
      {
        printf("Creating new seed file\n");
        fflush(NULL);
      }

      sprintf(oldtpt_name, "%s.%03d", tp_file, iter_count);
      if(debug == TRUE)
      {
        printf("   Opening old tie point file %s\n", oldtpt_name);
        fflush(NULL);
      }
      oldtpt_file = FOPEN(oldtpt_name, "r");
      if(oldtpt_file == NULL)
      {
	printf("ERROR: file %s could not be opened for reading!\n", oldtpt_name);
	exit(0);
      }
      
      sprintf(newtpt_name, "%s.%03d", tp_file, iter_count + 1);
      if(debug == TRUE)
      {
        printf("   Opening new tie point file %s\n", newtpt_name);
        fflush(NULL);
      }
      newtpt_file = FOPEN(newtpt_name, "w");
      if(newtpt_file == NULL)
      {
	printf("ERROR: file %s could not be opened for writing!\n", newtpt_name);
	exit(0);
      }

      if(debug == TRUE)
      {
        printf("   Copying file %s to %s\n", oldtpt_name, newtpt_name);
        fflush(NULL);
      }
      {
	int ii = 1, jj;
	char line_buf[256];

	while(fgets(line_buf, 256, oldtpt_file))
        {
	  int do_copy = TRUE;
	  if(debug == TRUE)
	  {
	    printf("      Grabbed line: %s", line_buf);
	    fflush(NULL);
	  }
	  for(jj = 0; jj < num_clip_points; ++jj)
	    if(ii == clip_list[jj])
	      do_copy = FALSE;

	  if(do_copy == TRUE)
	  {
	    if(debug == TRUE)
	    {
	      printf("      Copying line %d of %s to %s\n", ii, oldtpt_name, newtpt_name);
	      fflush(NULL);
	    }
	    fprintf(newtpt_file, "%s", line_buf);
	  }
	  else
	  {
	    if(debug == TRUE)
	    {
	      printf("      Skipping line %d of %s\n", ii, oldtpt_name);
	      fflush(NULL);
	    }
	    printf("   Clipping #%d: dif: %lf;   data: %s", ii, difs[ii - 1], line_buf);
	    fflush(NULL);
	  }

	  ++ii;
        }

	if(debug == TRUE)
	{
	  printf("   File copied\n");
	  fflush(NULL);
	}
      }
      free(clip_list);

      if(debug == TRUE)
      {
        printf("   Closing %s and %s\n", oldtpt_name, newtpt_name);
        fflush(NULL);
      }

      FCLOSE(oldtpt_file);
      FCLOSE(newtpt_file);

      if(fileNumLines(newtpt_name) <= orig_num_points * (max_rem * 0.01))
      {
	printf("Maximum allowed percentage of points removed (%2.2lf%%)\n"
	       "Halting iteration\n", max_rem);
	do_iter = FALSE;
      }
    }

    free(difs);

    if(keep == FALSE)
    {
      char rem_command[256];
      if(debug == TRUE)
      {
        printf("Removing temporary files from iteration %d\n", iter_count - 1);
        fflush(NULL);
      }
      sprintf(rem_command, "rm -f %s.%03d", oldbase_file, iter_count);
      printf("%s\n", rem_command);
      system(rem_command);
      if(iter_count > 0)
      {
        sprintf(rem_command, "rm -f %s.%03d", tp_file, iter_count - 1);
        printf("%s\n", rem_command);
        system(rem_command);
      }
    }
    if(debug == FALSE)
    {
      if(iter_count > 0)
      {
        char rem_command[256];
        sprintf(rem_command, "rm -f matrix.%d", iter_count - 1);
        printf("%s\n", rem_command);
        system(rem_command);
        sprintf(rem_command, "rm -f ctrlpts.%d", iter_count - 1);
        printf("%s\n", rem_command);
        system(rem_command);
        sprintf(rem_command, "rm -f vecB.%d", iter_count - 1);
        printf("%s\n", rem_command);
        system(rem_command);
      }
    }

    /* Increment the counter */
    if(debug == TRUE)
    {
      printf("Incrementing counter and iterating again\n");
      fflush(NULL);
    }
    ++iter_count;
    fflush(NULL);
  }

  if(iter_count >= max_iter)
    printf("You must have really horrible data, because we exceeded the maximum number of iterations (%d).\n", max_iter);

  /* Perform final clean up */
  {
    char temp[256];

    sprintf(temp, "cp %s.%03d %s", oldbase_file, iter_count, newbase_file);
    if(debug == TRUE)
    {
      printf("Copying last baseline %s.%03d to final location %s\n",
 	     oldbase_file, iter_count, newbase_file);
      fflush(NULL);
    }
    printf("%s\n", temp);
    system(temp);

    if(keep == FALSE)
    {
      char rem_command[256];
      sprintf(rem_command, "rm -f %s.%03d", oldbase_file, iter_count);
      printf("%s\n", rem_command);
      system(rem_command);
    }
    
    if(debug == FALSE)
    {
      if(iter_count > 0)
      {
        char rem_command[256];
        sprintf(rem_command, "rm -f matrix.%d", iter_count - 1);
        printf("%s\n", rem_command);
        system(rem_command);
        sprintf(rem_command, "rm -f ctrlpts.%d", iter_count - 1);
        printf("%s\n", rem_command);
        system(rem_command);
        sprintf(rem_command, "rm -f vecB.%d", iter_count - 1);
        printf("%s\n", rem_command);
        system(rem_command);
      }
    }
  }

  if(debug == TRUE)
  {
    printf("After %d iterations, program apparently executed successfully!\n\n", iter_count);
    fflush(NULL);
  }
  
  return 0;
}
