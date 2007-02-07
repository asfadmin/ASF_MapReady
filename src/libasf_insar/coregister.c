#include "asf.h"
#include "ardop_params.h"

int average_in_doppler(char *inFileMaster, char *inFileSlave, char *outFile)
{  
  FILE *fp;
  struct ARDOP_PARAMS ardop_master, ardop_slave;
  float avg_t1, avg_t2, avg_t3;

  // Read .in file into ardop structures 
  read_params(inFileMaster, &ardop_master);
  read_params(inFileSlave, &ardop_slave);
  
  avg_t1 = (ardop_master.fd + ardop_slave.fd) / 2;
  avg_t2 = (ardop_master.fdd + ardop_slave.fdd) / 2;
  avg_t3 = (ardop_master.fddd + ardop_slave.fddd) / 2;
  
  asfPrintStatus(logbuf,"\nAverage Doppler: %e %e %e \n\n", 
		 avg_t1, avg_t2, avg_t3);
  
  // Store result, in the order fd fdd fddd
  fp = FOPEN(outFile, "w");
  fprintf(fp, "%e %e %e", avg_t1, avg_t2, avg_t3);
  FCLOSE(fp); 
  
  return(0);
}
