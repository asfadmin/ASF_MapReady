/* Program which tries to ensure that the .meta file with the given
   base name is new-style.  If the .meta file associated with the given
   base name is old-style, a corresponding .ddr file is sought and
   used to update the .meta file in place.  */

#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>

#include <asf_meta.h>

char *program_name;		/* For error reporting.  */

int main(int argc, char **argv)
{
  meta_parameters *meta;	/* Metadata structure.  */

  program_name = basename(argv[0]);

  if ( argc != 2 ) {
    fprintf(stderr, "%s: wrong number of arguments\n", program_name);
    fprintf(stderr, "Usage: %s meta_file_basename\n", program_name);
    exit(EXIT_FAILURE);
  }
  
  meta = meta_read(argv[1]);
  meta_write(meta, argv[1]);
  
  exit(EXIT_SUCCESS);
}
  
