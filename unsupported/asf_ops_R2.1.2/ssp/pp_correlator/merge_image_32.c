/* SccsId[]= @(#)merge_image_32.c	2.41 3/24/98 */
static char sccsid_merge_image_32[]= "@(#)PPmerge_image_32.c:2.41";

#include "pp.h"

void merge_image_32(int my_processor_number, int number_of_processors,
		    int *image_buffer, int record_length,
		    int number_of_records)
{
  MPI_Status status;
  int step_size, right_neighbor, left_neighbor, tag, i, j, offset;
  char *tmp_buffer;
  int number_of_pixels;

  tag = 0;
  number_of_pixels = record_length/(2*sizeof(short));

  if((tmp_buffer = (char*)malloc(record_length)) == NULL) {
    info_handler(ierr_5, NULL, "cannot allocate %d bytes of memory",
		 record_length);
  }

  /* parallel merge using a tree algorithm */
  for(step_size = 1; step_size < number_of_processors; step_size*=2) { 
    if(my_processor_number%(2*step_size) == 0) {
      right_neighbor = my_processor_number+step_size;
      if(right_neighbor < number_of_processors) {

	/* get one row of image in range at a time. use blocked receive */
	offset = 0;
	for(j = 0; j < number_of_records; j++) {
	  MPI_Recv(tmp_buffer,record_length,MPI_BYTE,right_neighbor,tag,
		   MPI_COMM_WORLD,&status);
	  for(i = 0; i < number_of_pixels; i++) {
	    if(image_buffer[i+offset] == 0 && ((int*)tmp_buffer)[i] != 0) {
	      image_buffer[i+offset] = ((int*)tmp_buffer)[i];
	    }
	  }
	  offset += number_of_pixels;
	}

      }
    } else{
      if(my_processor_number%(step_size) == 0){
	left_neighbor = my_processor_number - step_size;
	if(left_neighbor >= 0) {

	  /* send one row of image in range at a time. use blocked send */
	  offset = 0;
	  for(j = 0; j < number_of_records; j++) {
	    MPI_Send(&image_buffer[offset],record_length,MPI_BYTE,
		     left_neighbor,tag,MPI_COMM_WORLD);
	    offset += number_of_pixels;
	  }

	}
      }
    }    
  }
  free(tmp_buffer);
}
