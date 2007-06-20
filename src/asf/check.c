#include "asf.h"

void check_return(int ret, char *msg)
{
  if (ret!=0) {
    asfPrintError("%s\n", msg);
  }
}


int check_status(char *status)
{
  if (strncmp(status, "new", 3)==0) return 1;
  else if (strncmp(status, "stop", 4)==0) {
    printf("   Processing interrupted by user!\n\n");
    exit(0);
  }
  else return 0;
}

int check_parameter(char parameter, check_input_t check)
{
  char str[255];
  int nParam;
  double fParam;

  if (check == FILE_EXISTS){

  } 
  if (check == FILE_TYPE) {
    
  }
}
