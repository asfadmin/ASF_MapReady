/* For testing out the parser (not in final program).  */

#include <stdio.h>
#include <stdlib.h>

#include "asf_meta.h"

/*Create new blank structure.  */
meta_parameters *raw_init(void)
{
	meta_parameters *meta = malloc(sizeof(meta_parameters));
	meta->general         = malloc(sizeof(meta_general));
	meta->sar             = malloc(sizeof(meta_sar));
	/*	meta->optical         = NULL;  /* Not yet in use */
	/*      meta->thermal         = NULL;  /* Not yet in use */
	meta->projection      = malloc(sizeof(meta_projection));
	/*      meta->stats           = NULL;  /* Not yet in use */
	meta->state_vectors   = malloc(sizeof(meta_state_vectors));
	meta->state_vectors->vecs = NULL;

	meta->geo  = NULL; /* DEPRECATED */
	meta->ifm  = NULL; /* DEPRECATED */
	meta->info = NULL; /* DEPRECATED */

	return meta;
}

int main(int argc, char **argv)
{
  meta_parameters *meta = raw_init();

  parse_metadata(meta, "test_file.meta");

  printf("Almost there...\n");
}
