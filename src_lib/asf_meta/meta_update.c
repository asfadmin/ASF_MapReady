/* Program which tries to ensure that the .meta file with the given
   base name is new-style.  If the .meta file associated with the given
   base name is old-style, a corresponding .ddr file is sought and
   used to update the .meta file in place.  */

#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <asf_meta.h>

#define MAX_FILE_NAME_LENGTH 250 /* Not including trailing null.  */
#define MAX_FILE_EXTENSION_LENGTH 5 /* Longest auto-appended extenstion.  */

char *program_name;		/* For error reporting.  */

void meta_update_usage(void)
{
  fprintf(stderr, "usage: %s [-o out_file_name] base_name\n", program_name);
  fprintf(stderr, "out_file_name, if present, should be the full name of the file to write\n");
  fprintf(stderr, "base_name should be the base name used by the .meta and .ddr input files\n");
}

int main(int argc, char **argv)
{
  meta_parameters *meta;	/* Metadata structure.  */
  int out_name_specified = 0;	/* Flag true iff outname given by option.  */
  char out_name[MAX_FILE_NAME_LENGTH + 1]; /* Name of output file.  */

  program_name = basename(argv[0]);

  /* Option processing.  */
  {
    char *opt_string = "o:";
    int opt_char;
    
    while ( (opt_char = getopt(argc, argv, opt_string)) != -1 ) {
      switch(opt_char) {
      case 'o':
	if ( strlen(optarg) > MAX_FILE_NAME_LENGTH )
	  fprintf(stderr, "%s: argumen to -o option too long\n", program_name);
	strncpy(out_name, optarg, MAX_FILE_NAME_LENGTH + 1);
	out_name_specified = 1;	/* Set flag true.  */
	break;
      case '?':
	meta_update_usage();
	exit(EXIT_FAILURE);
	break;
      default: 
	fprintf(stderr, "%s: unexpected error parsing command line\n",
		program_name);
	meta_update_usage();
	exit(EXIT_FAILURE);
	break;
      }
    }
  }

  if ( argc  - optind + 1 != 2 ) {
    fprintf(stderr, "%s: wrong number of arguments\n", program_name);
    fprintf(stderr, "Usage: %s meta_file_basename\n", program_name);
    exit(EXIT_FAILURE);
  }
  
  if ( strlen(argv[optind]) 
       > MAX_FILE_NAME_LENGTH - MAX_FILE_EXTENSION_LENGTH ) {
    fprintf(stderr, "%s: argument base name '%s' too long\n", program_name,
	    argv[optind]);
  }

  meta = meta_read(argv[optind]);

  if ( !out_name_specified ) {
    strncpy(out_name, argv[optind], MAX_FILE_NAME_LENGTH 
                            	    - MAX_FILE_EXTENSION_LENGTH);
    strncat(out_name, ".meta", MAX_FILE_EXTENSION_LENGTH + 1);
  }
  
  meta_write(meta, out_name);
  
  exit(EXIT_SUCCESS);
}
  
