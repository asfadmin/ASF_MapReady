/* SccsId[]= @(#)echo_data_histgram_4.c	2.41 3/24/98 */
static char sccsid_echo_data_histgram_4[]= "@(#)PPecho_data_histgram_4.c:2.41";

#include "pp.h"

void echo_data_histgram_4(int my_processor_number, int number_of_processors) 
{
  int i, j, n_levels, buffer_size, n_pulses, i_val, j_val, fd;
  int *i_hstgrm, *q_hstgrm;
  char *buffer;
  int block_size, remainder;
  char file_name[32];
  
  block_size = auxiliary.aux_echo_pulses/number_of_processors;
  remainder = auxiliary.aux_echo_pulses%number_of_processors;

  n_levels = 16;
  i_hstgrm = (int*) malloc(n_levels*sizeof(int));
  q_hstgrm = (int*) malloc(n_levels*sizeof(int));
  
  buffer_size = auxiliary.aux_echo_samples*sizeof(char);
  buffer = (char*) malloc(buffer_size);
  if(!buffer) {
    info_handler(ierr_5,NULL,"Cannot allocate %d bytes of memory",buffer_size);
  }
  
  for(i = 0; i < n_levels; i++) {
    i_hstgrm[i] = 0;
    q_hstgrm[i] = 0;
  }
  
  if(lseek(raw_file_fd,
	   raw_data_offset+my_processor_number*(block_size*buffer_size),
	   SEEK_SET)==-1) {
    info_handler(ierr_3,raw_file,"cannot lseek %d bytes in file %s.",
		 raw_data_offset+my_processor_number*(block_size*buffer_size), 
		 raw_file);
  }

  n_pulses = block_size;
  if(my_processor_number == number_of_processors-1) n_pulses += remainder;

  for(i = 0; i < n_pulses; i++) {
    if(read(raw_file_fd, buffer, buffer_size) != buffer_size) {
      info_handler(ierr_3,NULL,
		   "cannot read %d bytes from echo pulse %d in file %s",
		   buffer_size, i+my_processor_number*block_size, raw_file);
    }
    for(j = 0; j < auxiliary.aux_echo_samples; j++) {
      i_val = buffer[j] >> 4;  /* higher 4 bits */
      j_val = buffer[j] & 15;  /* lower 4 bits */
			
      if (i_val < 0) i_val = 0;
      if (j_val < 0) j_val = 0;

      i_hstgrm[i_val]++;
      q_hstgrm[j_val]++;
    }
  }

  /* global reduction.  reduced result is on processor zero */
  MPI_Reduce(i_hstgrm, i_hstgrm, n_levels, MPI_INT, MPI_SUM, 0,MPI_COMM_WORLD);
  MPI_Reduce(q_hstgrm, q_hstgrm, n_levels, MPI_INT, MPI_SUM, 0,MPI_COMM_WORLD);

  /* construct file name */
  sprintf(file_name, "/home/tmpdisk/hist.dat.%d", request.job_id);
  if(my_processor_number == 0) {
    fd = open(file_name, O_RDWR|O_CREAT|O_TRUNC, 0664);
    if(fd < 0) {
      info_handler(ierr_2, file_name, "open file %s failed",file_name);
    }
    write(fd, i_hstgrm, n_levels*sizeof(int));
    write(fd, q_hstgrm, n_levels*sizeof(int));
    close(fd);
    
    /*
    for(i = 0; i < n_levels; i++) {
      info_handler(0,NULL,"level %d, i_histgram %d", i, i_hstgrm[i]); 
      info_handler(0,NULL,"level %d, q_histgram %d", i, q_hstgrm[i]); 
    } 
    */
  } 

  free(i_hstgrm);
  free(q_hstgrm);
  free(buffer);
}
