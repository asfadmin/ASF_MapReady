/* SccsId[]= @(#)merge_image_8.c	2.41 3/24/98 */
static char sccsid_merge_image_8[]= "@(#)PPmerge_image_8.c:2.41";

#include "pp.h"

void merge_image_8(int my_processor_number,int number_of_processors,
		 char *image_buffer,int nfr, int nfx)
{
  MPI_Status status;
  int step_size, right_neighbor, left_neighbor, tag, i, j, offset;
  char *tmp_buffer;

  tag = 0;

  if((tmp_buffer = (char *)malloc(nfx*sizeof(char))) == NULL) {
    info_handler(ierr_5, NULL, "cannot allocate %d bytes of memory",nfx);
  }

  /* parallel merge using a tree algorithm */
  for(step_size = 1; step_size < number_of_processors; step_size*=2) { 
    if(my_processor_number%(2*step_size) == 0) {
      right_neighbor = my_processor_number+step_size;
      if(right_neighbor < number_of_processors) {

	/* get one row of image in azimuth at a time. use blocked receive */
	offset = 0;
	for(j = 0; j < nfr; j++) {
	  MPI_Recv(tmp_buffer,nfx,MPI_BYTE,right_neighbor,tag,
		   MPI_COMM_WORLD,&status);
	  for(i = 0; i < nfx; i++) {
	    if(image_buffer[i+offset] == 0 && tmp_buffer[i] != 0) {
	      image_buffer[i+offset] = tmp_buffer[i];
	    }
	  }
	  offset += nfx;
	}

      }
    } else{
      if(my_processor_number%(step_size) == 0){
	left_neighbor = my_processor_number - step_size;
	if(left_neighbor >= 0) {

	  /* send one row of image in azimuth at a time. use blocked send */
	  offset = 0;
	  for(j = 0; j < nfr; j++) {
	    MPI_Send(&image_buffer[offset],nfx,MPI_BYTE,left_neighbor,tag,
		     MPI_COMM_WORLD);
	    offset += nfx;
	  }

	}
      }
    }    
  }
  free(tmp_buffer);
}
