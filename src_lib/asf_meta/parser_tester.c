/* For testing out the parser (not in final program).  */

#include <stdio.h>
#include <stdlib.h>

#include "asf_meta.h"
#include "meta_init.h"

int main(int argc, char **argv)
{
  meta_parameters *meta = raw_init();

  parse_metadata(meta, "test_file.meta");

  printf("Almost there...\n");
}
