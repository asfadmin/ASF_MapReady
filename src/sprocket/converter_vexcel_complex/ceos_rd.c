/**** Functions to read CEOS records ****/

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#define INFO ( getenv("METADATA_DEBUG") != NULL)

double ceos_read_double ( int fd, int position, int length);
int ceos_read_int( int fd, int position, int length );
void ceos_read_char ( int fd, int position, int length, char * str );
int ceos_read_binary ( int fd, int position, int length);

/*+ Convert a 1 to 4 byte binary record value to an integer +*/

int ceos_read_binary ( int fd, /* input file */
                       int position, /* position in file */
                       int length) /* length of data record */
{
   unsigned char foo[25];
   ceos_read_char(fd, position, length, foo);
   
   switch ( length )
   {
      case 1:
         return ( (int) foo[0] );
      case 2:
         return ( (int)(foo[0])*256 + (int) (foo[1]) );
      case 3:
         return ( (int)(foo[0])*256*256 + (int) (foo[1])*256 + (int)(foo[2]) );
      case 4:
         return ( (int)(foo[0])*256*256*256
                 +(int)(foo[1])*256*256
                 +(int)(foo[2])*256
                 +(int)(foo[3])
                );
   }
   return 0;
}


#undef CALLER
#define CALLER "ceos_read_double"

/*+ Convert a binary data record to a double +*/

double ceos_read_double ( int fd, int position, int length)
{
   char buff[256];
   float ret = 0;
   
   
   ceos_read_char(fd, position, length, buff);
   
   sscanf(buff, "%g", &ret);
   
   if ( INFO )
   {
      
      printf("INFO (%s): {%x} = \"%s\"\n", CALLER, position, buff);
      fflush(NULL);
   }
   return (double)ret;
}

#undef CALLER
#define CALLER "ceos_read_int"

/*+ Convert an ascii data record to an int +*/

int ceos_read_int( int fd, int position, int length )
{
   
   char buff[256];
   
   ceos_read_char(fd, position, length, buff);
   if ( INFO )
   {
      
      printf("INFO (%s): {%x} = \"%s\"\n", CALLER, position, buff);
      fflush(NULL);
   }
   
   return atoi(buff);
   
}

#undef CALLER
#define CALLER "ceos_read_char"

/*+ Read a record datum and rerturn it as a string +*/

void ceos_read_char ( int fd, int position, int length, char * str )
{
   position = position - 1;
   /* jump to position.. */
   if ( lseek(fd, position, SEEK_SET) != position)
   {
      fprintf(stderr, "ERROR(%s):\t lseek failed.\n", CALLER);
      exit(-1);
   }
   
   /* read the data chunk */
   read ( fd, str, length);
   
   str[length] = 0;
   
   if ( INFO )
   {
      
      printf("INFO (%s): {%x} = \"%s\"\n", CALLER, position, str);
      fflush(NULL);
   }
}


/*
void main ()
{
int fd;

fd = open("/scratch2/test_images/vexcel/20126/lea.20126_slc", O_RDONLY);
if ( fd < 0 )
   printf("Error opening file ( %d:\"%s\")", errno,strerror(errno));
else
   {
printf("result:=%d\n", ceos_read_int(fd, 259, 6));
printf("result:=%g\n", ceos_read_double(fd, 720+133, 16));
}

}
 */
