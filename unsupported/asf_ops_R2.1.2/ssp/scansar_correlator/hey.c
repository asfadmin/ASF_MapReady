/* SccsId[]= @(#)hey.c	2.41 3/24/98 */
#include <stdio.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/ioctl.h>

#include <netinet/in.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <ctype.h>
#include <netdb.h>
#include <errno.h>
#include "error.h"
#include <unistd.h>

int lostconn();

main()

{
 int i;

  for (i=0;i<1000;i++)
  {
   printf("%d\n",i);
   fflush(stdout);
   sleep(1);
  signal(SIGPIPE, lostconn);
  }


}


lostconn()
 {
  printf("fuck\n");
  fflush(stdout);

 }
