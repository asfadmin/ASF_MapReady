/* SccsId[]= @(#)select_fdd_amb.c	2.41 3/24/98 */
static char sccsid_select_fdd[]= "@(#)PPselect_fdd_amb.c:2.41";

#include "pp.h"

void select_fdd_amb(double fd0_base_old, double fdd_old, double fr0_base_old, 
		double frd_old,	double fd_pred, int number_of_processors, 
		int my_processor_number, char *Dop_log_file)

{
  double *fd0_base_buff, *fdd_buff, *fr0_base_buff, *frd_buff;
  double *fd_pred_buff;
  int elem_count, count, i, j;
  double mean, std, closest;
  MPI_Status status;
  int tag;
  int *mask;

  FILE *fptr;

  elem_count = number_of_processors;

  fd0_base_buff = (double*) malloc(elem_count*sizeof(double));
  fdd_buff = (double*) malloc(elem_count*sizeof(double));
  fr0_base_buff = (double*) malloc(elem_count*sizeof(double));
  frd_buff = (double*) malloc(elem_count*sizeof(double));
  fd_pred_buff = (double*) malloc(elem_count*sizeof(double));
  mask = (int*) malloc(elem_count*sizeof(int));

  /* for some unknown reason, MPI_Gather causes segmentation fault later
     in the program 
  */
  /*
  MPI_Gather(&fd0_base, 1, MPI_DOUBLE, fd0_base_buff, 1, MPI_DOUBLE, 0, 
	     MPI_COMM_WORLD);
  MPI_Gather(&fdd, 1, MPI_DOUBLE, fdd_buff, 1, MPI_DOUBLE, 0, 
	     MPI_COMM_WORLD);
  MPI_Gather(&fr0_base, 1, MPI_DOUBLE, fr0_base_buff, 1, MPI_DOUBLE, 0, 
	     MPI_COMM_WORLD);
  MPI_Gather(&frd, 1, MPI_DOUBLE, frd_buff, 1, MPI_DOUBLE, 0, 
	     MPI_COMM_WORLD);
  */

  tag = 1;
  if(my_processor_number == 0) {
    fd0_base_buff[0] = fd0_base;
    for(i = 1; i < number_of_processors; i++) {
      MPI_Recv(&fd0_base_buff[i], sizeof(double), MPI_BYTE, i, 
	       tag, MPI_COMM_WORLD, &status);
    }
  } else {
    MPI_Send(&fd0_base, sizeof(double), MPI_BYTE, 0, tag, MPI_COMM_WORLD);
  }

  tag = 2;
  if(my_processor_number == 0) {
    fdd_buff[0] = fdd;
    for(i = 1; i < number_of_processors; i++) {
      MPI_Recv(&fdd_buff[i], sizeof(double), MPI_BYTE, i, tag, 
	       MPI_COMM_WORLD, &status);
    }
  } else {
    MPI_Send(&fdd, sizeof(double), MPI_BYTE, 0, tag, MPI_COMM_WORLD);
  }

  tag = 3;
  if(my_processor_number == 0) {
    fr0_base_buff[0] = fr0_base;
    for(i = 1; i < number_of_processors; i++) {
      MPI_Recv(&fr0_base_buff[i], sizeof(double), MPI_BYTE, 
	       i, tag, MPI_COMM_WORLD, &status);
    }
  } else {
    MPI_Send(&fr0_base, sizeof(double), MPI_BYTE, 0, tag, MPI_COMM_WORLD);
  }

  tag = 4;
  if(my_processor_number == 0) {
    frd_buff[0] = frd;
    for(i = 1; i < number_of_processors; i++) {
      MPI_Recv(&frd_buff[i], sizeof(double), MPI_BYTE, i, tag, 
	       MPI_COMM_WORLD, &status);
    }
  } else {
    MPI_Send(&frd, sizeof(double), MPI_BYTE, 0, tag, MPI_COMM_WORLD);
  }

  tag = 5;
  if(my_processor_number == 0) {
    fd_pred_buff[0] = fd_pred;
    for(i = 1; i < number_of_processors; i++) {
      MPI_Recv(&fd_pred_buff[i], sizeof(double), MPI_BYTE, i, tag, 
	       MPI_COMM_WORLD, &status);
    }
  } else {
    MPI_Send(&fd_pred, sizeof(double), MPI_BYTE, 0, tag, MPI_COMM_WORLD);
  }



  if(my_processor_number == 0) {
    /******** print out the selections for all fdd ****/
    info_handler(0,NULL,"QDN the predict value, fd0_base %f and fdd %f",
                        fd0_base_old, fdd_old);
    for(i=0;i<elem_count;i++) {
       info_handler(0,NULL,"QDN at process %d, fd0_base %f and fdd %f",
                    i,fd0_base_buff[i],fdd_buff[i],fd_pred_buff[i]);
    }

    /******** selection based on fdd *************/
    count = 0;
    for(i = 0; i < elem_count; i++) {
      if(fabs(fdd_buff[i]/fdd_old) >= 4.0 && 
         fabs(fdd_old)> 0.0001 && fdd_buff[i]*fdd_old <= 0.0) {
	mask[i] = 0;
	info_handler(0,NULL,"throw away processor %d's fd0_base %f and fdd %f",i,fd0_base_buff[i],fdd_buff[i]);
      } else {
	mask[i] = 1;
	count++;
      }
    }
    if(count == 0) {
      fd0_base = fd0_base_old;
      fdd = fdd_old;
      fr0_base = fr0_base_old;
      frd = frd_old;
	info_handler(0,NULL,"Have to use the default value, fd0_base %f and fdd %f",fd0_base_old,fdd_old);
      goto broadcast;
    }

    /*********** selection based on fd0_base *************/
    /* mean and standard deviation */
    mean = 0.0;
    std = 0.0;
    for(i = 0; i < elem_count; i++) {
      if(mask[i]) {
	mean += fd0_base_buff[i];
	std += fd0_base_buff[i]*fd0_base_buff[i];
      } 
    }
    mean /= count;
    std -= count*mean*mean;
    std = sqrt(fabs(std)/count);
    /* 
       take the absolute value so negative zero can be handled. also use
       count instead of count-1 for the denominator so that the code will 
       run on one processor. 
    */

    for(i = 0; i < elem_count; i++) {
      if(mask[i]) {
	if(fabs(fd0_base_buff[i] - mean) > std) {
	  mask[i] = 0;
	  info_handler(0,NULL,"throw away the out points %d's fd0_base %f and fdd %f",i,fd0_base_buff[i],fdd_buff[i]);
	}
      }
    }

    mean = 0;
    count = 0;
    for(i = 0; i < elem_count; i++) {
      if(mask[i]) {
	mean += fd0_base_buff[i];
	count++;
      }
    }
    mean /= count;
    
    /* use the first valid fd0_base as the closest */
    for(i = 0; i < elem_count; i++) {
      if(mask[i]) {
	closest = fabs(fd0_base_buff[i] - mean);
	break;
      }
    }
    /* find closest */
    j = 0;
    for(i = 0; i < elem_count; i++) {
      if(mask[i]) {
	if(fabs(fd0_base_buff[i] - mean) < closest) {
	  closest = fabs(fd0_base_buff[i] - mean);
	  j = i;
	}
      }
    }

    fd0_base = fd0_base_buff[j];
    fdd = fdd_buff[j];
    fr0_base = fr0_base_buff[j];
    frd = frd_buff[j];

    info_handler(0,NULL,"chose processor %d's fd0_base %f and fdd %f",
		 j, fd0_base, fdd); 

    
    /*for radarsat, we modify the doppler rate coefficient */
    if(request.platform[0] == 'R') {
      fr0_base = fr0_base*1.03;
      /* QDN 3/20/98, subtract the fd0_base by one PRF to meet loc spec. */
      fd0_base = fd0_base - prf;
      info_handler(0,NULL,
      "final select processor %d's (SUBTRACT ONE PRF) fd0_base %f and fdd %f",
		 j, fd0_base, fdd); 
      /* QDN 3/20/98, save the information about the doppler predict */
      fptr = fopen(Dop_log_file,"a+"); 
      /* QDN 3/23/98, try to open new file */
      if(fptr == NULL) {
         info_handler(0,NULL,
              "Can not append the Doppler log file %s",Dop_log_file);
         exit(1);
      } else {
         fprintf(fptr,
              "Rev= %d Frame_id= %d Mode= %s fd_pred= %lf fd0_base= %lf fd_diff= %lf prf= %lf\n",
              request.revolution,request.frame_id,request.instr_mode,
              fd_pred_buff[j],fd0_base,fd0_base-fd_pred_buff[j],prf);
         fflush(fptr);
         fclose(fptr);
      }
      
    }
  }

broadcast:

  MPI_Bcast(&fd0_base, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD); 
  MPI_Bcast(&fdd, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD); 
  MPI_Bcast(&fr0_base, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD); 
  MPI_Bcast(&frd, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD); 

  free(fd0_base_buff);
  free(fdd_buff);
  free(fr0_base_buff);
  free(frd_buff);
  free(fd_pred_buff);
  free(mask);
}


