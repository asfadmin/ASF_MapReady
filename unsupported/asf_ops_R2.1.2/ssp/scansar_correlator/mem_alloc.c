/* SccsId[]= @(#)mem_alloc.c	2.41 3/24/98 */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include "error.h"


int map_alloc(fnm,size1,ptr_image)
int *size1;
int *ptr_image;
char *fnm;
{
 float *IMAGE;
 int ifd_image,i;

    for (i = 0; i < strlen(fnm); i++)
      if (*(fnm + i) == ' ') *(fnm + i) = '\0';

       if( (ifd_image=open(fnm,2)) < 0 ) 
      {
       printf(" Can not open the input file temp_image\n");
       exit(1);
      }
     if( (IMAGE = (float *) mmap(0,*size1,
                   PROT_READ | PROT_WRITE , MAP_SHARED, ifd_image, 0)) == NULL) 
      {
         printf(" Can not memory mapped the input file \n");
         exit(1);
       }

 *ptr_image=IMAGE;
 close(ifd_image);

 return(1);
}
/**********************************************************/
